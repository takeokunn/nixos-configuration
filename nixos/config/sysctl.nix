{
  boot.kernel.sysctl."vm.swappiness" = 100;
  boot.kernel.sysctl."vm.vfs_cache_pressure" = 50;
  boot.kernel.sysctl."vm.dirty_ratio" = 10;
  boot.kernel.sysctl."vm.dirty_background_ratio" = 5;
  boot.kernel.sysctl."net.core.default_qdisc" = "fq";
  boot.kernel.sysctl."net.ipv4.tcp_congestion_control" = "bbr";
}
