# This file has been generated by node2nix 1.11.1. Do not edit!

{
  nodeEnv,
  fetchurl,
  fetchgit,
  nix-gitignore,
  stdenv,
  lib,
  globalBuildInputs ? [ ],
}:

let
  sources = {
    "@modelcontextprotocol/sdk-1.0.1" = {
      name = "_at_modelcontextprotocol_slash_sdk";
      packageName = "@modelcontextprotocol/sdk";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@modelcontextprotocol/sdk/-/sdk-1.0.1.tgz";
        sha512 = "slLdFaxQJ9AlRg+hw28iiTtGvShAOgOKXcD0F91nUcRYiOMuS9ZBYjcdNZRXW9G5JQ511GRTdUy1zQVZDpJ+4w==";
      };
    };
    "bytes-3.1.2" = {
      name = "bytes";
      packageName = "bytes";
      version = "3.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/bytes/-/bytes-3.1.2.tgz";
        sha512 = "/Nf7TyzTx6S3yRJObOAV7956r8cr2+Oj8AC5dt8wSP3BQAoeX58NoHyCU8P8zGkNXStjTSi6fzO6F0pBdcYbEg==";
      };
    };
    "content-type-1.0.5" = {
      name = "content-type";
      packageName = "content-type";
      version = "1.0.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/content-type/-/content-type-1.0.5.tgz";
        sha512 = "nTjqfcBFEipKdXCv4YDQWCfmcLZKm81ldF0pAopTvyrFGVbcR6P/VAAd5G7N+0tTr8QqiU0tFadD6FK4NtJwOA==";
      };
    };
    "depd-2.0.0" = {
      name = "depd";
      packageName = "depd";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/depd/-/depd-2.0.0.tgz";
        sha512 = "g7nH6P6dyDioJogAAGprGpCtVImJhpPk/roCzdb3fIh61/s/nPsfR6onyMwkCAR/OlC3yBC0lESvUoQEAssIrw==";
      };
    };
    "http-errors-2.0.0" = {
      name = "http-errors";
      packageName = "http-errors";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/http-errors/-/http-errors-2.0.0.tgz";
        sha512 = "FtwrG/euBzaEjYeRqOgly7G0qviiXoJWnvEH2Z1plBdXgbyjv34pHTSb9zoeHMyDy33+DWy5Wt9Wo+TURtOYSQ==";
      };
    };
    "iconv-lite-0.6.3" = {
      name = "iconv-lite";
      packageName = "iconv-lite";
      version = "0.6.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.6.3.tgz";
        sha512 = "4fCk79wshMdzMp2rH06qWrJE4iolqLhCUH+OiuIgU++RB0+94NlDL81atO7GX55uUKueo0txHNtvEyI6D7WdMw==";
      };
    };
    "inherits-2.0.4" = {
      name = "inherits";
      packageName = "inherits";
      version = "2.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz";
        sha512 = "k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==";
      };
    };
    "raw-body-3.0.0" = {
      name = "raw-body";
      packageName = "raw-body";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/raw-body/-/raw-body-3.0.0.tgz";
        sha512 = "RmkhL8CAyCRPXCE28MMH0z2PNWQBNk2Q09ZdxM9IOOXwxwZbN+qbWaatPkdkWIKL2ZVDImrN/pK5HTRz2PcS4g==";
      };
    };
    "safer-buffer-2.1.2" = {
      name = "safer-buffer";
      packageName = "safer-buffer";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz";
        sha512 = "YZo3K82SD7Riyi0E1EQPojLz7kpepnSQI9IyPbHHg1XXXevb5dJI7tpyN2ADxGcQbHG7vcyRHk0cbwqcQriUtg==";
      };
    };
    "setprototypeof-1.2.0" = {
      name = "setprototypeof";
      packageName = "setprototypeof";
      version = "1.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.2.0.tgz";
        sha512 = "E5LDX7Wrp85Kil5bhZv46j8jOeboKq5JMmYM3gVGdGH8xFpPWXUMsNrlODCrkoxMEeNi/XZIwuRvY4XNwYMJpw==";
      };
    };
    "statuses-2.0.1" = {
      name = "statuses";
      packageName = "statuses";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/statuses/-/statuses-2.0.1.tgz";
        sha512 = "RwNA9Z/7PrK06rYLIzFMlaF+l73iwpzsqRIFgbMLbTcLD6cOao82TaWefPXQvB2fOC4AjuYSEndS7N/mTCbkdQ==";
      };
    };
    "toidentifier-1.0.1" = {
      name = "toidentifier";
      packageName = "toidentifier";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/toidentifier/-/toidentifier-1.0.1.tgz";
        sha512 = "o5sSPKEkg/DIQNmH43V0/uerLrpzVedkUh8tGNvaeXpfpuwjKenlSox/2O/BTlZUtEe+JG7s5YhEz608PlAHRA==";
      };
    };
    "unpipe-1.0.0" = {
      name = "unpipe";
      packageName = "unpipe";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz";
        sha512 = "pjy2bYhSsufwWlKwPc+l3cN7+wuJlK6uz0YdJEOlQDbl6jo/YlPi4mb8agUkVC8BF7V8NuzeyPNqRksA3hztKQ==";
      };
    };
    "vscode-jsonrpc-8.2.0" = {
      name = "vscode-jsonrpc";
      packageName = "vscode-jsonrpc";
      version = "8.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-8.2.0.tgz";
        sha512 = "C+r0eKJUIfiDIfwJhria30+TYWPtuHJXHtI7J0YlOmKAo7ogxP20T0zxB7HZQIFhIyvoBPwWskjxrvAtfjyZfA==";
      };
    };
    "vscode-languageserver-protocol-3.17.5" = {
      name = "vscode-languageserver-protocol";
      packageName = "vscode-languageserver-protocol";
      version = "3.17.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.17.5.tgz";
        sha512 = "mb1bvRJN8SVznADSGWM9u/b07H7Ecg0I3OgXDuLdn307rl/J3A9YD6/eYOssqhecL27hK1IPZAsaqh00i/Jljg==";
      };
    };
    "vscode-languageserver-types-3.17.5" = {
      name = "vscode-languageserver-types";
      packageName = "vscode-languageserver-types";
      version = "3.17.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.17.5.tgz";
        sha512 = "Ld1VelNuX9pdF39h2Hgaeb5hEZM2Z3jUrrMgWQAu82jMtZp7p3vJT3BzToKtZI7NgQssZje5o0zryOrhQvzQAg==";
      };
    };
    "zod-3.24.2" = {
      name = "zod";
      packageName = "zod";
      version = "3.24.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/zod/-/zod-3.24.2.tgz";
        sha512 = "lY7CDW43ECgW9u1TcT3IoXHflywfVqDYze4waEz812jR/bZ8FHDsl7pFQoSZTz5N+2NqRXs8GBwnAwo3ZNxqhQ==";
      };
    };
  };
in
{
  "@github/copilot-language-server" = nodeEnv.buildNodePackage {
    name = "_at_github_slash_copilot-language-server";
    packageName = "@github/copilot-language-server";
    version = "1.296.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/@github/copilot-language-server/-/copilot-language-server-1.296.0.tgz";
      sha512 = "0uNNCsoHbpSM5AsVmSwLktVmJrwNBlfnTl+GUICLIelyOvQdYJ19ceb84YxtuR+udxiN3+yX4rJzv/o9xmmmyQ==";
    };
    dependencies = [
      sources."vscode-jsonrpc-8.2.0"
      sources."vscode-languageserver-protocol-3.17.5"
      sources."vscode-languageserver-types-3.17.5"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Your AI pair programmer";
      homepage = "https://github.com/github/copilot-language-server-release";
      license = "https://docs.github.com/en/site-policy/github-terms/github-terms-for-additional-products-and-features";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  "@anthropic-ai/claude-code" = nodeEnv.buildNodePackage {
    name = "_at_anthropic-ai_slash_claude-code";
    packageName = "@anthropic-ai/claude-code";
    version = "0.2.64";
    src = fetchurl {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-0.2.64.tgz";
      sha512 = "qwXePRKDi1P/r1ya96tGXN4agHX4OnAtHj5kkL1W17CKJP+JZa64izCQL2z/iEXvA4KV59gWiYogBHzFgBqH3Q==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "Use Claude, Anthropic's AI assistant, right from your terminal. Claude can understand your codebase, edit files, run terminal commands, and handle entire workflows for you.";
      homepage = "https://github.com/anthropics/claude-code";
      license = "SEE LICENSE IN README.md";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  "@modelcontextprotocol/server-brave-search" = nodeEnv.buildNodePackage {
    name = "_at_modelcontextprotocol_slash_server-brave-search";
    packageName = "@modelcontextprotocol/server-brave-search";
    version = "0.6.2";
    src = fetchurl {
      url = "https://registry.npmjs.org/@modelcontextprotocol/server-brave-search/-/server-brave-search-0.6.2.tgz";
      sha512 = "AtdnPh8zVsEooXWZD21Negz3JL6iRmKf4sUtwCrLe0e83QBJD6Hf5B3rOFkwsrnTew/6xL7oyRkhc6YuXhuuhQ==";
    };
    dependencies = [
      sources."@modelcontextprotocol/sdk-1.0.1"
      sources."bytes-3.1.2"
      sources."content-type-1.0.5"
      sources."depd-2.0.0"
      sources."http-errors-2.0.0"
      sources."iconv-lite-0.6.3"
      sources."inherits-2.0.4"
      sources."raw-body-3.0.0"
      sources."safer-buffer-2.1.2"
      sources."setprototypeof-1.2.0"
      sources."statuses-2.0.1"
      sources."toidentifier-1.0.1"
      sources."unpipe-1.0.0"
      sources."zod-3.24.2"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "MCP server for Brave Search API integration";
      homepage = "https://modelcontextprotocol.io";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}
