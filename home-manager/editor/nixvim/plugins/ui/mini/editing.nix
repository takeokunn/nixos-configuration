{
  plugins.mini.modules = {
    pairs = { };

    surround = {
      mappings = {
        add = "sa";
        delete = "sd";
        find = "sf";
        find_left = "sF";
        highlight = "sh";
        replace = "sr";
        update_n_lines = "sn";
      };
      n_lines = 50;
      respect_selection_type = true;
    };

    jump = {
      mappings = {
        forward = "f";
        backward = "F";
        forward_till = "t";
        backward_till = "T";
        repeat_jump = ";";
      };
    };

    jump2d = {
      spotter = {
        __raw = "require('mini.jump2d').builtin_opts.word_start.spotter";
      };
      view = {
        dim = true;
        n_steps_ahead = 2;
      };
    };

    # Enhanced around/inside textobjects: o=block, f=function, c=class, t=tag, d=digit, e=word, u=function_call
    ai = {
      n_lines = 500;
      custom_textobjects.__raw = ''
        {
          o = require('mini.ai').gen_spec.treesitter({
            a = { '@block.outer', '@conditional.outer', '@loop.outer' },
            i = { '@block.inner', '@conditional.inner', '@loop.inner' },
          }, {}),
          f = require('mini.ai').gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }, {}),
          c = require('mini.ai').gen_spec.treesitter({ a = '@class.outer', i = '@class.inner' }, {}),
          t = { '<([%p%w]-)%f[^<%w][^<>]->.-</%1>' },
          d = { '%f[%d]%d+' },
          e = { '%u[%l%d]+%f[^%l%d]', '%f[%S][%l%d]+%f[^%l%d]', '%f[%P][%l%d]+%f[^%l%d]', '^[%l%d]+%f[^%l%d]' },
          u = require('mini.ai').gen_spec.function_call(),
          U = require('mini.ai').gen_spec.function_call({ name_pattern = '[%w_]' }),
        }
      '';
    };

    # Using Shift modifier to avoid conflict with LSP (Alt+j/k)
    move = {
      mappings = {
        left = "<M-h>";
        right = "<M-l>";
        down = "<M-S-j>";
        up = "<M-S-k>";
        line_left = "<M-h>";
        line_right = "<M-l>";
        line_down = "<M-S-j>";
        line_up = "<M-S-k>";
      };
    };

    splitjoin = {
      mappings = {
        toggle = "gS";
      };
    };

    operators = {
      evaluate = {
        prefix = "g=";
      };
      exchange = {
        prefix = "gx";
      };
      multiply = {
        prefix = "gm";
      };
      replace = {
        prefix = "gr";
      };
      sort = {
        prefix = "gs";
      };
    };

    align = { };

    comment = {
      mappings = {
        comment = "gc";
        comment_line = "gcc";
        comment_visual = "gc";
        textobject = "gc";
      };
    };

    # cursor/resize disabled for performance (constant CPU overhead)
    animate = {
      cursor = {
        enable = false;
      };
      scroll = {
        enable = true;
        timing.__raw = "require('mini.animate').gen_timing.linear({ duration = 80, unit = 'total' })";
        subscroll.__raw = ''
          require('mini.animate').gen_subscroll.equal({
            predicate = function(total_scroll) return math.abs(total_scroll) > 5 end
          })
        '';
      };
      resize = {
        enable = false;
      };
      open = {
        enable = true;
        timing.__raw = "require('mini.animate').gen_timing.linear({ duration = 120, unit = 'total' })";
      };
      close = {
        enable = true;
        timing.__raw = "require('mini.animate').gen_timing.linear({ duration = 120, unit = 'total' })";
      };
    };
  };

  keymaps = [
    # mini.jump2d keymap - using gj to avoid conflict with window navigation (sh/sj/sk/sl)
    {
      mode = [
        "n"
        "x"
        "o"
      ];
      key = "gj";
      action.__raw = "function() require('mini.jump2d').start(require('mini.jump2d').builtin_opts.single_character) end";
      options.desc = "Jump to character (2d)";
    }
  ];
}
