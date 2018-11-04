{nixops}: nixops.overrideAttrs (_: { patches = [ ./vultr.patch ]; })
