{
  lib,
  config,
  ...
}:
let
  cfg = config.plugins.oil;
in
{
  config = lib.mkIf cfg.enable {
    extraConfigLua = ''
      -- Git status cache for oil.nvim
      local function parse_output(proc)
        local result = proc:wait()
        local ret = {}
        if result.code == 0 then
          for line in vim.gsplit(result.stdout, "\n", { plain = true, trimempty = true }) do
            line = line:gsub("/$", "")
            ret[line] = true
          end
        end
        return ret
      end

      local function new_git_status()
        return setmetatable({}, {
          __index = function(self, key)
            local ignore_proc = vim.system(
              { "git", "ls-files", "--ignored", "--exclude-standard", "--others", "--directory" },
              { cwd = key, text = true }
            )
            local tracked_proc = vim.system(
              { "git", "ls-tree", "HEAD", "--name-only" },
              { cwd = key, text = true }
            )
            local ret = {
              ignored = parse_output(ignore_proc),
              tracked = parse_output(tracked_proc),
            }
            rawset(self, key, ret)
            return ret
          end,
        })
      end

      git_status = new_git_status()

      local refresh = require("oil.actions").refresh
      local orig_refresh = refresh.callback
      refresh.callback = function(...)
        git_status = new_git_status()
        orig_refresh(...)
      end
    '';
  };
}
