# This file was generated by nvfetcher, please do not modify it manually.
{
  fetchgit,
  fetchurl,
  fetchFromGitHub,
  dockerTools,
}:
{
  dracula-fish = {
    pname = "dracula-fish";
    version = "269cd7d76d5104fdc2721db7b8848f6224bdf554";
    src = fetchFromGitHub {
      owner = "dracula";
      repo = "fish";
      rev = "269cd7d76d5104fdc2721db7b8848f6224bdf554";
      fetchSubmodules = false;
      sha256 = "sha256-Hyq4EfSmWmxwCYhp3O8agr7VWFAflcUe8BUKh50fNfY=";
    };
    date = "2023-06-23";
  };
  dracula-sublime = {
    pname = "dracula-sublime";
    version = "456d3289827964a6cb503a3b0a6448f4326f291b";
    src = fetchFromGitHub {
      owner = "dracula";
      repo = "sublime";
      rev = "456d3289827964a6cb503a3b0a6448f4326f291b";
      fetchSubmodules = false;
      sha256 = "sha256-8mCovVSrBjtFi5q+XQdqAaqOt3Q+Fo29eIDwECOViro=";
    };
    date = "2023-08-30";
  };
  dracula-tig = {
    pname = "dracula-tig";
    version = "e8a3387d8353e90cca41f5d89c3e1f74f1f7c8c6";
    src = fetchFromGitHub {
      owner = "dracula";
      repo = "tig";
      rev = "e8a3387d8353e90cca41f5d89c3e1f74f1f7c8c6";
      fetchSubmodules = false;
      sha256 = "sha256-PnBuQJWCqARvjZg/Mfi7imcTa+I4VYvnYSt+GGMzxCQ=";
    };
    date = "2022-03-20";
  };
  emacs-bazel-mode = {
    pname = "emacs-bazel-mode";
    version = "769b30dc18282564d614d7044195b5a0c1a0a5f3";
    src = fetchFromGitHub {
      owner = "bazelbuild";
      repo = "emacs-bazel-mode";
      rev = "769b30dc18282564d614d7044195b5a0c1a0a5f3";
      fetchSubmodules = false;
      sha256 = "sha256-riB6U1gTuYdStwzejvlsSneMYYFSDvdUFNdaFx3l8BA=";
    };
    date = "2023-09-19";
  };
  emacs-consult-tramp = {
    pname = "emacs-consult-tramp";
    version = "befa62baca768caa457b167e773b91f1bc7d661f";
    src = fetchFromGitHub {
      owner = "Ladicle";
      repo = "consult-tramp";
      rev = "befa62baca768caa457b167e773b91f1bc7d661f";
      fetchSubmodules = false;
      sha256 = "sha256-Ddeat4FfQS+5BvAP41xNsNw3bzTJv41xuaIA5a44Kvc=";
    };
    date = "2022-03-17";
  };
  emacs-copilot = {
    pname = "emacs-copilot";
    version = "7d105d708a23d16cdfd5240500be8bb02f95a46e";
    src = fetchFromGitHub {
      owner = "copilot-emacs";
      repo = "copilot.el";
      rev = "7d105d708a23d16cdfd5240500be8bb02f95a46e";
      fetchSubmodules = false;
      sha256 = "sha256-ATIyNAnd1pU82xdGH7+z9zkOcDVJhYhh65QDQ73NiL0=";
    };
    date = "2025-02-23";
  };
  emacs-copilot-chat = {
    pname = "emacs-copilot-chat";
    version = "d379abce9e3f878187a2c7126035caceca560feb";
    src = fetchFromGitHub {
      owner = "chep";
      repo = "copilot-chat.el";
      rev = "d379abce9e3f878187a2c7126035caceca560feb";
      fetchSubmodules = false;
      sha256 = "sha256-Yzu9Xubkxzt8KDJi/gRJr+HJ69wZeMPWP22MJa6zAJY=";
    };
    date = "2025-03-19";
  };
  emacs-direnv-mode = {
    pname = "emacs-direnv-mode";
    version = "c0bf3b81c7a97e2a0d06d05495e86848254fcc1f";
    src = fetchFromGitHub {
      owner = "wbolster";
      repo = "emacs-direnv";
      rev = "c0bf3b81c7a97e2a0d06d05495e86848254fcc1f";
      fetchSubmodules = false;
      sha256 = "sha256-g500rooMfkVX22u/HNELT+rKv0+q51Xg4k4R48fPyKw=";
    };
    date = "2024-03-14";
  };
  emacs-eglot-booster = {
    pname = "emacs-eglot-booster";
    version = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";
    src = fetchFromGitHub {
      owner = "jdtsmith";
      repo = "eglot-booster";
      rev = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";
      fetchSubmodules = false;
      sha256 = "sha256-PLfaXELkdX5NZcSmR1s/kgmU16ODF8bn56nfTh9g6bs=";
    };
    date = "2024-10-29";
  };
  emacs-eshell-multiple = {
    pname = "emacs-eshell-multiple";
    version = "342c36ef9c71df8738f4435fd4381f506631e7aa";
    src = fetchFromGitHub {
      owner = "takeokunn";
      repo = "eshell-multiple";
      rev = "342c36ef9c71df8738f4435fd4381f506631e7aa";
      fetchSubmodules = false;
      sha256 = "sha256-+4x8Xkaqj44rcvrqv/3M8p+b842c6uLNBGPMaDtQUbs=";
    };
    date = "2022-08-17";
  };
  emacs-explain-pause-mode = {
    pname = "emacs-explain-pause-mode";
    version = "ac3eb69f36f345506aad05a6d9bc3ef80d26914b";
    src = fetchFromGitHub {
      owner = "lastquestion";
      repo = "explain-pause-mode";
      rev = "ac3eb69f36f345506aad05a6d9bc3ef80d26914b";
      fetchSubmodules = false;
      sha256 = "sha256-6FDYnE9rT12f2Lx3yg2tpDVm9txF2VoVxZvADpm4BoM=";
    };
    date = "2025-02-01";
  };
  emacs-fish-repl = {
    pname = "emacs-fish-repl";
    version = "5dd66957e494ea201de6e2b0c934dbac6f12743a";
    src = fetchFromGitHub {
      owner = "takeokunn";
      repo = "fish-repl.el";
      rev = "5dd66957e494ea201de6e2b0c934dbac6f12743a";
      fetchSubmodules = false;
      sha256 = "sha256-6clzUsB7dllXKe5CeT0kwZl5Cjs9KKhPFaDa9B0aUHE=";
    };
    date = "2021-09-24";
  };
  emacs-flycheck-projectile = {
    pname = "emacs-flycheck-projectile";
    version = "ce6e9e8793a55dace13d5fa13badab2dca3b5ddb";
    src = fetchFromGitHub {
      owner = "nbfalcon";
      repo = "flycheck-projectile";
      rev = "ce6e9e8793a55dace13d5fa13badab2dca3b5ddb";
      fetchSubmodules = false;
      sha256 = "sha256-p3/y9iTfgYKJyRNXF2cB7Jam36QWIq4/ZUB+G+YRHjQ=";
    };
    date = "2020-10-31";
  };
  emacs-llm = {
    pname = "emacs-llm";
    version = "9cbd2220107d50c47a759b91d142321b88ec32a9";
    src = fetchFromGitHub {
      owner = "ahyatt";
      repo = "llm";
      rev = "9cbd2220107d50c47a759b91d142321b88ec32a9";
      fetchSubmodules = false;
      sha256 = "sha256-1Wd7s/pviBILAJyRKo8xPwxmM8mEsDGikE3u52ogVzQ=";
    };
    date = "2025-03-18";
  };
  emacs-mu4e-dashboard = {
    pname = "emacs-mu4e-dashboard";
    version = "c9c09b7ed6433070de148b656ac273b7fb7cec07";
    src = fetchFromGitHub {
      owner = "rougier";
      repo = "mu4e-dashboard";
      rev = "c9c09b7ed6433070de148b656ac273b7fb7cec07";
      fetchSubmodules = false;
      sha256 = "sha256-bCelxaT+qaR2W80Cr591A4cRycIFJmXjeY8/aqIpl5g=";
    };
    date = "2024-01-22";
  };
  emacs-ob-fish = {
    pname = "emacs-ob-fish";
    version = "4cf4b8bcd58e8fad45d8f690fbdf9cafc2958748";
    src = fetchFromGitHub {
      owner = "takeokunn";
      repo = "ob-fish";
      rev = "4cf4b8bcd58e8fad45d8f690fbdf9cafc2958748";
      fetchSubmodules = false;
      sha256 = "sha256-BMi6NN1ZXr2EcvPCtIcUz9RKtrA045tYYZUPM1FQao4=";
    };
    date = "2023-03-24";
  };
  emacs-ob-phpstan = {
    pname = "emacs-ob-phpstan";
    version = "c77355e70703affffb166b7100b9e9f3efb21c6e";
    src = fetchFromGitHub {
      owner = "emacs-php";
      repo = "ob-phpstan";
      rev = "c77355e70703affffb166b7100b9e9f3efb21c6e";
      fetchSubmodules = false;
      sha256 = "sha256-UTWlpszxXC1sue6E72bIeL+1NKzJ+VC1WQBXOZD0mwI=";
    };
    date = "2024-03-27";
  };
  emacs-ob-racket = {
    pname = "emacs-ob-racket";
    version = "c7b7eee58fcde2ad515b72288742e555e7ec7915";
    src = fetchFromGitHub {
      owner = "hasu";
      repo = "emacs-ob-racket";
      rev = "c7b7eee58fcde2ad515b72288742e555e7ec7915";
      fetchSubmodules = false;
      sha256 = "sha256-yv+PP1JyEvMxEToNbgDbgWih/GHdauwfYLzPaEPsEC8=";
    };
    date = "2024-05-31";
  };
  emacs-ob-treesitter = {
    pname = "emacs-ob-treesitter";
    version = "c3fac35f95dcaffdb90836c606d119717c43238d";
    src = fetchFromGitHub {
      owner = "takeokunn";
      repo = "ob-treesitter";
      rev = "c3fac35f95dcaffdb90836c606d119717c43238d";
      fetchSubmodules = false;
      sha256 = "sha256-3lroCj3FhRS1wgb/UVHYO4CjgP1rsicqB/rARvzsfoc=";
    };
    date = "2024-01-06";
  };
  emacs-org-dashboard = {
    pname = "emacs-org-dashboard";
    version = "02c0699771d199075a286e4502340ca6e7c9e831";
    src = fetchFromGitHub {
      owner = "bard";
      repo = "org-dashboard";
      rev = "02c0699771d199075a286e4502340ca6e7c9e831";
      fetchSubmodules = false;
      sha256 = "sha256-OWwRHMFJvxIKwu+6wdsd+b2zRTX3Q0nbEQi7sl8fIn4=";
    };
    date = "2017-12-23";
  };
  emacs-org-view-mode = {
    pname = "emacs-org-view-mode";
    version = "16d7c87a1bef54abd892f0fda2a541043c42e097";
    src = fetchFromGitHub {
      owner = "amno1";
      repo = "org-view-mode";
      rev = "16d7c87a1bef54abd892f0fda2a541043c42e097";
      fetchSubmodules = false;
      sha256 = "sha256-bLRUHFzVA6+04iY1tnmFQzBRgW6P09+gK9it8D8QrVA=";
    };
    date = "2024-02-18";
  };
  emacs-org-volume = {
    pname = "emacs-org-volume";
    version = "caa30d5b958c9f37854d7ab35c99445c00bc7d1e";
    src = fetchFromGitHub {
      owner = "akirak";
      repo = "org-volume";
      rev = "caa30d5b958c9f37854d7ab35c99445c00bc7d1e";
      fetchSubmodules = false;
      sha256 = "sha256-J1DdP10uc6KeWl+ZhsEBifEJ99lDyKlmcuInHa5p3/M=";
    };
    date = "2024-07-23";
  };
  emacs-ox-hatena = {
    pname = "emacs-ox-hatena";
    version = "24777234566f5472b0e8b3c5faeb2e045fd91e12";
    src = fetchFromGitHub {
      owner = "zonkyy";
      repo = "ox-hatena";
      rev = "24777234566f5472b0e8b3c5faeb2e045fd91e12";
      fetchSubmodules = false;
      sha256 = "sha256-gQ6oU2t/xlyTK4FRInqeHd9AH+vRpCBM/aMbpn1tHTU=";
    };
    date = "2013-08-23";
  };
  emacs-ox-typst = {
    pname = "emacs-ox-typst";
    version = "8f25cf584b764943e94dd715c410eba0835e054e";
    src = fetchFromGitHub {
      owner = "jmpunkt";
      repo = "ox-typst";
      rev = "8f25cf584b764943e94dd715c410eba0835e054e";
      fetchSubmodules = false;
      sha256 = "sha256-UTfEqEmek8bcYQJ/eg1tWpBn+zjQiGO4DvvMIBPIvQQ=";
    };
    date = "2025-03-17";
  };
  emacs-php-doc-block = {
    pname = "emacs-php-doc-block";
    version = "bdf1ddba2cadd52ee7dd5691baefc6306ea62c81";
    src = fetchFromGitHub {
      owner = "moskalyovd";
      repo = "emacs-php-doc-block";
      rev = "bdf1ddba2cadd52ee7dd5691baefc6306ea62c81";
      fetchSubmodules = false;
      sha256 = "sha256-pmCOsKcKcFNZP/ipj5bj9IAK+Ulthso+HQKpzakRCzA=";
    };
    date = "2020-03-23";
  };
  emacs-pinentry = {
    pname = "emacs-pinentry";
    version = "77991a491f3fc4673dfdd959ba6b4c9b0111150f";
    src = fetchFromGitHub {
      owner = "ueno";
      repo = "pinentry-el";
      rev = "77991a491f3fc4673dfdd959ba6b4c9b0111150f";
      fetchSubmodules = false;
      sha256 = "sha256-IEVvu9/G0Nl0CH8e+Vj+KRWvtUxKMMAMgz+2NhftTsY=";
    };
    date = "2024-11-23";
  };
  emacs-plz-event-source = {
    pname = "emacs-plz-event-source";
    version = "79e726da2e697f99c8f1196e4badabac6ae3176b";
    src = fetchFromGitHub {
      owner = "r0man";
      repo = "plz-event-source";
      rev = "79e726da2e697f99c8f1196e4badabac6ae3176b";
      fetchSubmodules = false;
      sha256 = "sha256-5cPC2bYOaRU8aEpSpcho8d4TYF0Firu0/p6kBlbPBh8=";
    };
    date = "2025-02-28";
  };
  emacs-plz-media-type = {
    pname = "emacs-plz-media-type";
    version = "ab69de91ae93ab6650175fdf0c4bb394d698b49a";
    src = fetchFromGitHub {
      owner = "r0man";
      repo = "plz-media-type";
      rev = "ab69de91ae93ab6650175fdf0c4bb394d698b49a";
      fetchSubmodules = false;
      sha256 = "sha256-BJQHsLflMu8/3oXdZ+sVFyeOiwc2Y8emw5MYMmGp6Os=";
    };
    date = "2025-02-28";
  };
  emacs-rainbow-csv = {
    pname = "emacs-rainbow-csv";
    version = "c005ea8c031d2f3917a378a02b6e352caff2f983";
    src = fetchFromGitHub {
      owner = "emacs-vs";
      repo = "rainbow-csv";
      rev = "c005ea8c031d2f3917a378a02b6e352caff2f983";
      fetchSubmodules = false;
      sha256 = "sha256-bKuL87/vqXtepB+h/kjsJw6jD95NUzf/k9FhE7Y8a18=";
    };
    date = "2025-02-26";
  };
  emacs-sudden-death = {
    pname = "emacs-sudden-death";
    version = "791a63d3f4df192e71f4232a9a4c5588f4b43dfb";
    src = fetchFromGitHub {
      owner = "yewton";
      repo = "sudden-death.el";
      rev = "791a63d3f4df192e71f4232a9a4c5588f4b43dfb";
      fetchSubmodules = false;
      sha256 = "sha256-+h6nWW9upcwWfIvYaF4It38+ouhqeBttm1dVbxpvanw=";
    };
    date = "2018-02-17";
  };
  emacs-systemd-mode = {
    pname = "emacs-systemd-mode";
    version = "8742607120fbc440821acbc351fda1e8e68a8806";
    src = fetchFromGitHub {
      owner = "holomorph";
      repo = "systemd-mode";
      rev = "8742607120fbc440821acbc351fda1e8e68a8806";
      fetchSubmodules = false;
      sha256 = "sha256-oj/E+b3oS/2QNNxTYDZ5Zwq/OHKI2FgN/eRV5EAexrE=";
    };
    date = "2023-01-31";
  };
  emacs-typst-mode = {
    pname = "emacs-typst-mode";
    version = "5776fd4f3608350ff6a2b61b118d38165d342aa3";
    src = fetchFromGitHub {
      owner = "Ziqi-Yang";
      repo = "typst-mode.el";
      rev = "5776fd4f3608350ff6a2b61b118d38165d342aa3";
      fetchSubmodules = false;
      sha256 = "sha256-mqkcNDgx7lc6kUSFFwSATRT+UcOglkeu+orKLiU9Ldg=";
    };
    date = "2023-09-25";
  };
  emacs-view-lock-mode = {
    pname = "emacs-view-lock-mode";
    version = "508b1a4b6d5e040455535331244104f5122e340b";
    src = fetchFromGitHub {
      owner = "s-fubuki";
      repo = "view-lock-mode";
      rev = "508b1a4b6d5e040455535331244104f5122e340b";
      fetchSubmodules = false;
      sha256 = "sha256-WUhQO/4O0NBpUfNofGUsS7Rfbel8iEGk+qCyOs9o0Ik=";
    };
    date = "2024-07-06";
  };
  emacs-web-php-blade-mode = {
    pname = "emacs-web-php-blade-mode";
    version = "a4463d2732caa8c3650826ee4fc79f3fd29c9e56";
    src = fetchFromGitHub {
      owner = "takeokunn";
      repo = "web-php-blade-mode";
      rev = "a4463d2732caa8c3650826ee4fc79f3fd29c9e56";
      fetchSubmodules = false;
      sha256 = "sha256-hZKMck0xJWTub81yCupCie+Z8FFFFP26IRm7uN/mbTI=";
    };
    date = "2022-06-22";
  };
  emacs-zalgo-mode = {
    pname = "emacs-zalgo-mode";
    version = "dc42228ce38db4f9879d6d53ba68207f2a5f7474";
    src = fetchFromGitHub {
      owner = "nehrbash";
      repo = "zalgo-mode";
      rev = "dc42228ce38db4f9879d6d53ba68207f2a5f7474";
      fetchSubmodules = false;
      sha256 = "sha256-R1fidCbailFsZZsQWNCznXqLuY3mG4bVL7rlxb1N2sg=";
    };
    date = "2024-10-05";
  };
  fish-artisan-completion = {
    pname = "fish-artisan-completion";
    version = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
    src = fetchFromGitHub {
      owner = "adriaanzon";
      repo = "fish-artisan-completion";
      rev = "8e8d726b3862fcb972abb652fb8c1a9fb9207a64";
      fetchSubmodules = false;
      sha256 = "sha256-+LKQVuWORJcyuL/YZ3B86hpbV4rbSkj41Y9qgwXZXu4=";
    };
    date = "2021-11-16";
  };
  fish-autopair = {
    pname = "fish-autopair";
    version = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
    src = fetchFromGitHub {
      owner = "jorgebucaran";
      repo = "autopair.fish";
      rev = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
      fetchSubmodules = false;
      sha256 = "sha256-qt3t1iKRRNuiLWiVoiAYOu+9E7jsyECyIqZJ/oRIT1A=";
    };
    date = "2022-07-04";
  };
  fish-bd = {
    pname = "fish-bd";
    version = "ab686e028bfe95fa561a4f4e57840e36902d4d7d";
    src = fetchFromGitHub {
      owner = "0rax";
      repo = "fish-bd";
      rev = "ab686e028bfe95fa561a4f4e57840e36902d4d7d";
      fetchSubmodules = false;
      sha256 = "sha256-GeWjoakXa0t2TsMC/wpLEmsSVGhHFhBVK3v9eyQdzv0=";
    };
    date = "2022-03-04";
  };
  fish-dart-completions = {
    pname = "fish-dart-completions";
    version = "f52734d3bbb79f362aa6541b490f74df49f124ff";
    src = fetchFromGitHub {
      owner = "takeokunn";
      repo = "fish-dart-completions";
      rev = "f52734d3bbb79f362aa6541b490f74df49f124ff";
      fetchSubmodules = false;
      sha256 = "sha256-CSvMkY5ObtAowr+PsPtJxsWaTZENgP5HrUU/PUoMtOw=";
    };
    date = "2021-09-25";
  };
  fish-done = {
    pname = "fish-done";
    version = "eb32ade85c0f2c68cbfcff3036756bbf27a4f366";
    src = fetchFromGitHub {
      owner = "franciscolourenco";
      repo = "done";
      rev = "eb32ade85c0f2c68cbfcff3036756bbf27a4f366";
      fetchSubmodules = false;
      sha256 = "sha256-DMIRKRAVOn7YEnuAtz4hIxrU93ULxNoQhW6juxCoh4o=";
    };
    date = "2024-04-11";
  };
  fish-ghq = {
    pname = "fish-ghq";
    version = "cafaaabe63c124bf0714f89ec715cfe9ece87fa2";
    src = fetchFromGitHub {
      owner = "decors";
      repo = "fish-ghq";
      rev = "cafaaabe63c124bf0714f89ec715cfe9ece87fa2";
      fetchSubmodules = false;
      sha256 = "sha256-6b1zmjtemNLNPx4qsXtm27AbtjwIZWkzJAo21/aVZzM=";
    };
    date = "2021-07-16";
  };
  fish-nix-completions = {
    pname = "fish-nix-completions";
    version = "cd8a43bed96e0acc02228bc77502be8ba5fa0548";
    src = fetchFromGitHub {
      owner = "kidonng";
      repo = "nix-completions.fish";
      rev = "cd8a43bed96e0acc02228bc77502be8ba5fa0548";
      fetchSubmodules = false;
      sha256 = "sha256-spnLmde41qQt8uJZFwiH0igFuVqZ6SvkwdA9Kbe2yz8=";
    };
    date = "2022-08-18";
  };
  fish-nix-env = {
    pname = "fish-nix-env";
    version = "7b65bd228429e852c8fdfa07601159130a818cfa";
    src = fetchFromGitHub {
      owner = "lilyball";
      repo = "nix-env.fish";
      rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
      fetchSubmodules = false;
      sha256 = "sha256-RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
    };
    date = "2021-11-29";
  };
  sublime-gleam = {
    pname = "sublime-gleam";
    version = "ff9638511e05b0aca236d63071c621977cffce38";
    src = fetchFromGitHub {
      owner = "molnarmark";
      repo = "sublime-gleam";
      rev = "ff9638511e05b0aca236d63071c621977cffce38";
      fetchSubmodules = false;
      sha256 = "sha256-94moZz9r5cMVPWTyzGlbpu9p2p/5Js7/KV6V4Etqvbo=";
    };
    date = "2020-10-25";
  };
  vim-denippet = {
    pname = "vim-denippet";
    version = "9a19bb6ddcdeb7a60860ac27e180d6d400ab10c7";
    src = fetchFromGitHub {
      owner = "uga-rosa";
      repo = "denippet.vim";
      rev = "9a19bb6ddcdeb7a60860ac27e180d6d400ab10c7";
      fetchSubmodules = false;
      sha256 = "sha256-98EGTcsh6wRkAcYKUBj2ar6HkCZav6wAt54xYrS/pJY=";
    };
    date = "2024-06-27";
  };
  vim-denippet-autoimport-vscode = {
    pname = "vim-denippet-autoimport-vscode";
    version = "81c09c53a61e4b42234fee43ce26f0ceb196b9b7";
    src = fetchFromGitHub {
      owner = "ryoppippi";
      repo = "denippet-autoimport-vscode";
      rev = "81c09c53a61e4b42234fee43ce26f0ceb196b9b7";
      fetchSubmodules = false;
      sha256 = "sha256-g3W4Ski+8dkY9fMfnFpi2Irytou4qSqb6Gess7LN7aI=";
    };
    date = "2024-05-06";
  };
  vim-fern = {
    pname = "vim-fern";
    version = "60d9a56380123198fe318c250d29a4a44bb952e9";
    src = fetchFromGitHub {
      owner = "lambdalisue";
      repo = "vim-fern";
      rev = "60d9a56380123198fe318c250d29a4a44bb952e9";
      fetchSubmodules = false;
      sha256 = "sha256-QA/Q1rlMbWJJa3IEqjhvf4/2i3RHPirwlwMtMTuKLnQ=";
    };
    date = "2025-02-18";
  };
  vim-fern-renderer-nerdfont = {
    pname = "vim-fern-renderer-nerdfont";
    version = "325629c68eb543229715b68920fbcb92b206beb6";
    src = fetchFromGitHub {
      owner = "lambdalisue";
      repo = "vim-fern-renderer-nerdfont";
      rev = "325629c68eb543229715b68920fbcb92b206beb6";
      fetchSubmodules = false;
      sha256 = "sha256-bcFIyPHxdckmmEGSCr9F5hLGTENF+KgRoz2BK49rGv4=";
    };
    date = "2024-07-13";
  };
  vim-friendly-snippets = {
    pname = "vim-friendly-snippets";
    version = "efff286dd74c22f731cdec26a70b46e5b203c619";
    src = fetchFromGitHub {
      owner = "rafamadriz";
      repo = "friendly-snippets";
      rev = "efff286dd74c22f731cdec26a70b46e5b203c619";
      fetchSubmodules = false;
      sha256 = "sha256-I8SRZxnoNC6SOWW+scoA77Jwyxcb4eUczppLdyOiZe0=";
    };
    date = "2024-12-01";
  };
  vim-gin = {
    pname = "vim-gin";
    version = "8a826cb3c6810c830553e606fe584e08e807606b";
    src = fetchFromGitHub {
      owner = "lambdalisue";
      repo = "vim-gin";
      rev = "8a826cb3c6810c830553e606fe584e08e807606b";
      fetchSubmodules = false;
      sha256 = "sha256-E2/L/YWzrSOvIc5FrZeeaybfA+tEB6gpWdnuQ6WzzXQ=";
    };
    date = "2025-01-02";
  };
  vim-neco = {
    pname = "vim-neco";
    version = "7b722cd13a44645e4ed9b1e4fe6ff8ad64f947f6";
    src = fetchFromGitHub {
      owner = "Shougo";
      repo = "neco-vim";
      rev = "7b722cd13a44645e4ed9b1e4fe6ff8ad64f947f6";
      fetchSubmodules = false;
      sha256 = "sha256-uBUl7DTCSRN8/YGiFKSUzTAg+BllvQ8PsTy7nzul10c=";
    };
    date = "2025-03-06";
  };
  vim-nerdfont = {
    pname = "vim-nerdfont";
    version = "3605ba4ba4dc0295f5eb400506fd05b451df3e1f";
    src = fetchFromGitHub {
      owner = "lambdalisue";
      repo = "vim-nerdfont";
      rev = "3605ba4ba4dc0295f5eb400506fd05b451df3e1f";
      fetchSubmodules = false;
      sha256 = "sha256-srTetFxbf1ZltYCBXB308WayMupRwQMJLd7Up7kYEuU=";
    };
    date = "2025-02-21";
  };
  vim-skkeleton = {
    pname = "vim-skkeleton";
    version = "f1f0586c69d84c6ee5f3e5a4ca76cc188161b944";
    src = fetchFromGitHub {
      owner = "vim-skk";
      repo = "skkeleton";
      rev = "f1f0586c69d84c6ee5f3e5a4ca76cc188161b944";
      fetchSubmodules = false;
      sha256 = "sha256-egAD1VDmoks9WwsEMOgyJmiMRV+p4f6S4sWOMKAew48=";
    };
    date = "2025-02-09";
  };
  vim-skkeleton-azik = {
    pname = "vim-skkeleton-azik";
    version = "3fcb3e91099257dea59c799e63410f5d5a2d67fb";
    src = fetchFromGitHub {
      owner = "kei-s16";
      repo = "skkeleton-azik-kanatable";
      rev = "3fcb3e91099257dea59c799e63410f5d5a2d67fb";
      fetchSubmodules = false;
      sha256 = "sha256-H7wOEyptxKtW10+vIv9cehi9PQlgc2PvD6U/cBqx560=";
    };
    date = "2024-11-23";
  };
  vimdoc-ja = {
    pname = "vimdoc-ja";
    version = "ff710d919b9c325c524ec15949d05b8d9cd61c06";
    src = fetchFromGitHub {
      owner = "vim-jp";
      repo = "vimdoc-ja";
      rev = "ff710d919b9c325c524ec15949d05b8d9cd61c06";
      fetchSubmodules = false;
      sha256 = "sha256-7Bh40qBcxbiyvmGtPzqEDV5WI74cDDaBi/3j6J61HJQ=";
    };
    date = "2025-03-17";
  };
}
