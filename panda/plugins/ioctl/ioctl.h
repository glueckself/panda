#include "panda/plugin.h"
#include "panda/common.h"

#include<osi/osi_types.h>
#include<osi/osi_ext.h>

#include <vector>
#include <unordered_map>
#include <unordered_set>

#define INLINE __attribute__ ((always_inline)) inline

// https://www.kernel.org/doc/html/latest/userspace-api/ioctl/ioctl-decoding.html
// https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/ioctl.h

#if defined(TARGET_PPC)
    #define IOC_SIZE_BITS 13
    #define IOC_TYPE_BITS  3
#else
    #define IOC_SIZE_BITS 14
    #define IOC_TYPE_BITS  2
#endif

#define IOC_CODE_BITS 8
#define IOC_FUNC_BITS 8

static const char* ioctl_type_strs[] {
    "IO",   // ioctl with no parameters
    "IOW",  // ioctl with read parameters  (copy_to_user)
    "IOR",  // ioctl with write parameters (copy_from_user)
    "IOWR", // ioctl with both write and read parameters
};

INLINE const char* ioctl_type_to_str(uint32_t type) {
    assert(type <= 0x3);
    return ioctl_type_strs[type];
}

typedef struct ioctl_cmd_t {
    uint32_t type : IOC_TYPE_BITS;
    uint32_t arg_size : IOC_SIZE_BITS;
    uint32_t code : IOC_CODE_BITS;
    uint32_t func_num : IOC_FUNC_BITS;
} ioctl_cmd_t;

typedef struct ioctl_t {
    char* file_name;
    ioctl_cmd_t* cmd;
    uint64_t guest_arg_ptr;
    uint8_t* guest_arg_buf;
} ioctl_t;

INLINE void decode_ioctl_cmd(ioctl_cmd_t* cmd, uint32_t val) {
    cmd->type     = val & ((1 << IOC_TYPE_BITS) - 1);
    cmd->arg_size   = (val >> IOC_TYPE_BITS) & ((1 << IOC_SIZE_BITS) - 1);
    cmd->code       = (val >> (IOC_TYPE_BITS + IOC_SIZE_BITS)) & ((1 << IOC_CODE_BITS) - 1);
    cmd->func_num   = (val >> (IOC_TYPE_BITS + IOC_SIZE_BITS + IOC_CODE_BITS)) & ((1 << IOC_FUNC_BITS) - 1);
}

INLINE int32_t encode_ioctl_cmd(ioctl_cmd_t* cmd) {
    return cmd->type
        | cmd->arg_size << IOC_TYPE_BITS
        | cmd->code << (IOC_TYPE_BITS + IOC_SIZE_BITS)
        | cmd->func_num << (IOC_TYPE_BITS + IOC_SIZE_BITS + IOC_CODE_BITS);
}

bool operator==(const ioctl_cmd_t &cmd_1, const ioctl_cmd_t &cmd_2) {
    return (cmd_1.type == cmd_2.type) &&
            (cmd_1.arg_size == cmd_2.arg_size) &&
            (cmd_1.code == cmd_2.code) &&
            (cmd_1.func_num == cmd_2.func_num);
}

// Pair of ioctl request and corresponding response
typedef std::pair<ioctl_t*, ioctl_t*> IoctlReqRet;

// List of request/response pairs
typedef std::vector<IoctlReqRet> AllIoctls;

// Map of PID to request/response lists
typedef std::unordered_map<target_pid_t, AllIoctls> AllIoctlsByPid;

// Map PID to process name
typedef std::unordered_map<target_pid_t, std::string> NameByPid;