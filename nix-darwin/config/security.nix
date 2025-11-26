{
  security = {
    pam.services.sudo_local = {
      touchIdAuth = true;
      reattach = true;
    };
  };
}
