{
  programs.bottom.enable = true;
  programs.bottom.settings = {
    row = [
      {
        child = [
          {
            ratio = 2;
            type = "cpu";
          }
          { type = "mem"; }
        ];
      }
      {
        child = [
          {
            ratio = 2;
            type = "net";
          }
          {
            ratio = 2;
            type = "disk";
          }
          { type = "temp"; }
        ];
      }
      {
        ratio = 2;
        child = [
          {
            default = true;
            type = "proc";
          }
        ];
      }
    ];
    disk.name_filter = {
      is_list_ignored = true;
      list = [ "/dev/loop\\d+" ];
      regex = true;
    };
    flags = {
      basic = false;
      dot_marker = false;
      table_gap = "none";
      rate = 700;
    };
    processes = {
      case_sensitive = false;
      default_grouped = true;
    };
  };
}
