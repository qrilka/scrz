Vagrant.configure("2") do |config|

  config.vm.box     = "raring-server-cloudimg-vagrant-amd64-disk1"
  config.vm.box_url = "http://cloud-images.ubuntu.com/raring/current/raring-server-cloudimg-vagrant-amd64-disk1.box"

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
  end

  config.vm.provision :shell, inline: <<-SCRIPT
    # Install necessary packages
    apt-get -y update
    apt-get -y install lxc ghc libcurl4-openssl-dev cabal-install btrfs-tools \
      debootstrap

    # Create btrfs filesystem inside a file, loop mount it to /srv/scrz
    fallocate -l 20G /srv/scrz.img
    losetup /dev/loop0 /srv/scrz.img
    mkfs.btrfs /dev/loop0
    losetup -d /dev/loop0
    mkdir /srv/scrz
    echo "/srv/scrz.img /srv/scrz btrfs loop 0 0" >> /etc/fstab
    mount /srv/scrz
  SCRIPT

end
