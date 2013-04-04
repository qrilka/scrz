module Scrz.LXC where

import Scrz.Types
import Scrz.Network.IPv4

lxcConfig :: String -> IPv4 -> IPv4 -> String -> String
lxcConfig hostname addr gatewayAddress rootfsPath = unlines
  [ "lxc.network.type   = veth"
  , "lxc.network.flags  = up"
  , "lxc.network.link   = scrz"
  , "lxc.network.name   = eth0"
  , "lxc.network.mtu    = 1500"
  , "lxc.network.ipv4   = " ++ (show addr) ++ "/24"
  , "lxc.network.ipv4.gateway   = " ++ (show gatewayAddress)
  , ""
  , "lxc.arch           = amd64"
  , "lxc.utsname        = " ++ hostname
  , ""
  , "lxc.console        = none"
  , "lxc.tty            = 1"
  , "lxc.devttydir      = lxc"
  , ""
  , "lxc.pts            = 1024"
  , "lxc.rootfs         = " ++ rootfsPath
  , ""
  , "lxc.mount.entry    = proc   " ++ rootfsPath ++ "/proc proc nosuid,nodev,noexec 0 0"
  , "lxc.mount.entry    = sysfs  " ++ rootfsPath ++ "/sys sysfs nosuid,nodev,noexec 0 0"
  , "lxc.mount.entry    = devpts " ++ rootfsPath ++ "/dev/pts devpts newinstance,ptmxmode=0666,nosuid,noexec 0 0"
  , "lxc.mount.entry    = run    " ++ rootfsPath ++ "/run tmpfs nosuid,nodev 0 0"
  , ""
  , "lxc.cap.drop = sys_module mac_admin mac_override"
  , ""
  , "lxc.cgroup.devices.deny = a"
  , ""
  , "# Allow any mknod (but not using the node)"
  , "lxc.cgroup.devices.allow = c *:* m"
  , "lxc.cgroup.devices.allow = b *:* m"
  , ""
  , "lxc.cgroup.devices.allow = c 1:3 rwm   # /dev/null"
  , "lxc.cgroup.devices.allow = c 1:5 rwm   # /dev/zero"
  , "lxc.cgroup.devices.allow = c 1:7 rwm   # /dev/full"
  , "lxc.cgroup.devices.allow = c 1:8 rwm   # /dev/random"
  , "lxc.cgroup.devices.allow = c 1:9 rwm   # /dev/urandom"
  , ""
  , "lxc.cgroup.devices.allow = c 4:0 rwm   # /dev/tty0"
  , "lxc.cgroup.devices.allow = c 4:1 rwm   # /dev/tty1"
  , ""
  , "lxc.cgroup.devices.allow = c 5:0 rwm   # /dev/tty"
  , "lxc.cgroup.devices.allow = c 5:1 rwm   # /dev/console"
  , "lxc.cgroup.devices.allow = c 5:2 rwm   # /dev/ptmx"
  , ""
  , "lxc.cgroup.devices.allow = c 10:229 rwm   # /dev/fuse"
  , "lxc.cgroup.devices.allow = c 10:200 rwm   # /dev/tun"
  , "lxc.cgroup.devices.allow = c 10:228 rwm   # /dev/hpet"
  , "lxc.cgroup.devices.allow = c 10:232 rwm   # /dev/kvm"
  , ""
  , "lxc.cgroup.devices.allow = c 136:* rwm   # /dev/{0,1,2,3,4}"
  , "lxc.cgroup.devices.allow = c 254:0 rwm   # /dev/rtc0"
  ]
