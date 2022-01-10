-module(ct_containers_docker_tests).

-include_lib("eunit/include/eunit.hrl").

docker_engine_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun returns_expected_status/1, fun returns_expected_ports/1]}.

setup() ->
    meck:new(ct_containers_http),
    meck:expect(ct_containers_http, get, ['_'], {200, container_info()}),
    meck:expect(ct_containers_http, url_encode, ['_'], <<"someurl">>),
    ok.

teardown(_) ->
    meck:unload(ct_containers_http).

returns_expected_status(_) ->
    [?_assertEqual({ok, <<"running">>}, ct_containers_docker:status(<<"id">>))].

returns_expected_ports(_) ->
    [?_assertEqual({ok, 49155}, ct_containers_docker:port(<<"id">>, {1234, tcp})),
     ?_assertEqual({error, no_port}, ct_containers_docker:port(<<"id">>, {12334, tcp}))].

container_info() ->
    #{<<"AppArmorProfile">> => <<>>,
      <<"Args">> =>
          [<<"/usr/sbin/mosquitto">>, <<"-c">>, <<"/mosquitto/config/mosquitto.conf">>],
      <<"Config">> =>
          #{<<"AttachStderr">> => false,
            <<"AttachStdin">> => false,
            <<"AttachStdout">> => false,
            <<"Cmd">> =>
                [<<"/usr/sbin/mosquitto">>, <<"-c">>, <<"/mosquitto/config/mosquitto.conf">>],
            <<"Domainname">> => <<>>,
            <<"Entrypoint">> => [<<"/docker-entrypoint.sh">>],
            <<"Env">> =>
                [<<"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin">>,
                 <<"VERSION=1.6.15">>,
                 <<"DOWNLOAD_SHA256=5ff2271512f745bf1a451072cd3768a5daed71e90c5179fae12b049d6c02aa0f">>,
                 <<"GPG_KEYS=A0D6EEA1DCAE49A635A3B2F0779B22DFB3E717B7">>,
                 <<"LWS_VERSION=4.2.1">>,
                 <<"LWS_SHA256=842da21f73ccba2be59e680de10a8cce7928313048750eb6ad73b6fa50763c51">>],
            <<"ExposedPorts">> => #{<<"1883/tcp">> => #{}},
            <<"Hostname">> => <<"7853cf0bbe92">>,
            <<"Image">> => <<"eclipse-mosquitto:1.6">>,
            <<"Labels">> =>
                #{<<"description">> => <<"Eclipse Mosquitto MQTT Broker">>,
                  <<"maintainer">> => <<"Roger Light <roger@atchoo.org>">>},
            <<"OnBuild">> => null,
            <<"OpenStdin">> => false,
            <<"StdinOnce">> => false,
            <<"Tty">> => false,
            <<"User">> => <<>>,
            <<"Volumes">> => #{<<"/mosquitto/data">> => #{}, <<"/mosquitto/log">> => #{}},
            <<"WorkingDir">> => <<>>},
      <<"Created">> => <<"2021-10-17T17:58:08.492019391Z">>,
      <<"Driver">> => <<"overlay2">>,
      <<"ExecIDs">> => null,
      <<"GraphDriver">> =>
          #{<<"Data">> =>
                #{<<"LowerDir">> =>
                      <<"/var/lib/docker/overlay2/89ff362e2cf71d16f00d63f51bd35f7bee8f0fa2b7687220eff5675ecde3b3ba-init/diff:/var/lib/docker/overlay2/0ddada12f460e4c5fdb7f4469741da6eb35b2f305252a1ea7de4fa116c34e8ee/diff:/var/lib/docker/overlay2/586fa14addf41e69914a1bfea1d993716fb05a118133a5e6af4d597891eac785/diff:/var/lib/docker/overlay2/b79d0885ee93c37e6c72169b2408f8d5f351b6495aa06d5c362d004bc18faf74/diff">>,
                  <<"MergedDir">> =>
                      <<"/var/lib/docker/overlay2/89ff362e2cf71d16f00d63f51bd35f7bee8f0fa2b7687220eff5675ecde3b3ba/merged">>,
                  <<"UpperDir">> =>
                      <<"/var/lib/docker/overlay2/89ff362e2cf71d16f00d63f51bd35f7bee8f0fa2b7687220eff5675ecde3b3ba/diff">>,
                  <<"WorkDir">> =>
                      <<"/var/lib/docker/overlay2/89ff362e2cf71d16f00d63f51bd35f7bee8f0fa2b7687220eff5675ecde3b3ba/work">>},
            <<"Name">> => <<"overlay2">>},
      <<"HostConfig">> =>
          #{<<"PidMode">> => <<>>,
            <<"ReadonlyPaths">> =>
                [<<"/proc/bus">>,
                 <<"/proc/fs">>,
                 <<"/proc/irq">>,
                 <<"/proc/sys">>,
                 <<"/proc/sysrq-trigger">>],
            <<"BlkioWeightDevice">> => null,
            <<"CpuRealtimeRuntime">> => 0,
            <<"GroupAdd">> => null,
            <<"VolumeDriver">> => <<>>,
            <<"CpuShares">> => 0,
            <<"PidsLimit">> => null,
            <<"SecurityOpt">> => null,
            <<"CapDrop">> => null,
            <<"ConsoleSize">> => [0, 0],
            <<"CpuQuota">> => 0,
            <<"AutoRemove">> => false,
            <<"CgroupParent">> => <<>>,
            <<"DeviceCgroupRules">> => null,
            <<"BlkioDeviceWriteBps">> => null,
            <<"CpusetMems">> => <<>>,
            <<"MemoryReservation">> => 0,
            <<"ShmSize">> => 67108864,
            <<"BlkioDeviceReadBps">> => null,
            <<"DnsOptions">> => null,
            <<"Isolation">> => <<>>,
            <<"MaskedPaths">> =>
                [<<"/proc/asound">>,
                 <<"/proc/acpi">>,
                 <<"/proc/kcore">>,
                 <<"/proc/keys">>,
                 <<"/proc/latency_stats">>,
                 <<"/proc/timer_list">>,
                 <<"/proc/timer_stats">>,
                 <<"/proc/sched_debug">>,
                 <<"/proc/scsi">>,
                 <<"/sys/firmware">>],
            <<"IOMaximumBandwidth">> => 0,
            <<"VolumesFrom">> => null,
            <<"NanoCpus">> => 0,
            <<"RestartPolicy">> => #{<<"MaximumRetryCount">> => 0, <<"Name">> => <<>>},
            <<"UTSMode">> => <<>>,
            <<"BlkioWeight">> => 0,
            <<"OomKillDisable">> => null,
            <<"CgroupnsMode">> => <<"private">>,
            <<"CpusetCpus">> => <<>>,
            <<"Privileged">> => false,
            <<"Devices">> => null,
            <<"BlkioDeviceReadIOps">> => null,
            <<"Cgroup">> => <<>>,
            <<"IOMaximumIOps">> => 0,
            <<"PortBindings">> => null,
            <<"CpuPeriod">> => 0,
            <<"DeviceRequests">> => null,
            <<"Dns">> => null,
            <<"MemorySwappiness">> => null,
            <<"CpuPercent">> => 0,
            <<"ExtraHosts">> => null,
            <<"Binds">> => null,
            <<"UsernsMode">> => <<>>,
            <<"NetworkMode">> => <<"default">>,
            <<"Runtime">> => <<"runc">>,
            <<"ReadonlyRootfs">> => false,
            <<"CapAdd">> => null,
            <<"KernelMemory">> => 0,
            <<"Ulimits">> => null,
            <<"MemorySwap">> => 0,
            <<"ContainerIDFile">> => <<>>,
            <<"KernelMemoryTCP">> => 0,
            <<"CpuCount">> => 0,
            <<"OomScoreAdj">> => 0,
            <<"PublishAllPorts">> => false,
            <<"Links">> => null,
            <<"LogConfig">> => #{<<"Config">> => #{}, <<"Type">> => <<"json-file">>},
            <<"DnsSearch">> => null,
            <<"IpcMode">> => <<"private">>,
            <<"CpuRealtimePeriod">> => 0,
            <<"BlkioDeviceWriteIOps">> => null,
            <<"Memory">> => 0},
      <<"HostnamePath">> =>
          <<"/var/lib/docker/containers/7853cf0bbe920586f8e1c73fa9c9da9ffc2751efbca2561a7b9143b6f24e5641/hostname">>,
      <<"HostsPath">> =>
          <<"/var/lib/docker/containers/7853cf0bbe920586f8e1c73fa9c9da9ffc2751efbca2561a7b9143b6f24e5641/hosts">>,
      <<"Id">> => <<"7853cf0bbe920586f8e1c73fa9c9da9ffc2751efbca2561a7b9143b6f24e5641">>,
      <<"Image">> =>
          <<"sha256:89bd2876099489348606dd8aa508a85e7691441c23d1f9a7a12d0f70120bd24f">>,
      <<"LogPath">> =>
          <<"/var/lib/docker/containers/7853cf0bbe920586f8e1c73fa9c9da9ffc2751efbca2561a7b9143b6f24e5641/7853cf0bbe920586f8e1c73fa9c9da9ffc2751efbca2561a7b9143b6f24e5641-json.log">>,
      <<"MountLabel">> => <<>>,
      <<"Mounts">> =>
          [#{<<"Destination">> => <<"/mosquitto/data">>,
             <<"Driver">> => <<"local">>,
             <<"Mode">> => <<>>,
             <<"Name">> => <<"660c342f7785f123d54cebe21a7a83328786d44796ca9c999e11c4193d48a34b">>,
             <<"Propagation">> => <<>>,
             <<"RW">> => true,
             <<"Source">> =>
                 <<"/var/lib/docker/volumes/660c342f7785f123d54cebe21a7a83328786d44796ca9c999e11c4193d48a34b/_data">>,
             <<"Type">> => <<"volume">>},
           #{<<"Destination">> => <<"/mosquitto/log">>,
             <<"Driver">> => <<"local">>,
             <<"Mode">> => <<>>,
             <<"Name">> => <<"fac05ec81465c39135c162a64e9ff68e877229ac743be7bb73a429927bff7405">>,
             <<"Propagation">> => <<>>,
             <<"RW">> => true,
             <<"Source">> =>
                 <<"/var/lib/docker/volumes/fac05ec81465c39135c162a64e9ff68e877229ac743be7bb73a429927bff7405/_data">>,
             <<"Type">> => <<"volume">>}],
      <<"Name">> => <<"/infallible_taussig">>,
      <<"NetworkSettings">> =>
          #{<<"Bridge">> => <<>>,
            <<"EndpointID">> =>
                <<"5270be3ce676e5ee63bd2d08d45fb7c882fb1f21cf9aa0aedd451d29237db66b">>,
            <<"Gateway">> => <<"172.17.0.1">>,
            <<"GlobalIPv6Address">> => <<>>,
            <<"GlobalIPv6PrefixLen">> => 0,
            <<"HairpinMode">> => false,
            <<"IPAddress">> => <<"172.17.0.2">>,
            <<"IPPrefixLen">> => 16,
            <<"IPv6Gateway">> => <<>>,
            <<"LinkLocalIPv6Address">> => <<>>,
            <<"LinkLocalIPv6PrefixLen">> => 0,
            <<"MacAddress">> => <<"02:42:ac:11:00:02">>,
            <<"Networks">> =>
                #{<<"bridge">> =>
                      #{<<"Aliases">> => null,
                        <<"DriverOpts">> => null,
                        <<"EndpointID">> =>
                            <<"5270be3ce676e5ee63bd2d08d45fb7c882fb1f21cf9aa0aedd451d29237db66b">>,
                        <<"Gateway">> => <<"172.17.0.1">>,
                        <<"GlobalIPv6Address">> => <<>>,
                        <<"GlobalIPv6PrefixLen">> => 0,
                        <<"IPAMConfig">> => null,
                        <<"IPAddress">> => <<"172.17.0.2">>,
                        <<"IPPrefixLen">> => 16,
                        <<"IPv6Gateway">> => <<>>,
                        <<"Links">> => null,
                        <<"MacAddress">> => <<"02:42:ac:11:00:02">>,
                        <<"NetworkID">> =>
                            <<"8ab25fbc35a693ac88c0a7c9bc104b8127df1ed8f0e46dcdaf0b4e27e08d0eeb">>}},
            <<"Ports">> =>
                #{<<"1234/tcp">> =>
                      [#{<<"HostIp">> => <<"0.0.0.0">>, <<"HostPort">> => <<"49155">>},
                       #{<<"HostIp">> => <<"::">>, <<"HostPort">> => <<"49155">>}]},
            <<"SandboxID">> =>
                <<"f4ed548570c7709b15db1827ea057bc0b2cb25091dc87e888b732d2dba9adda7">>,
            <<"SandboxKey">> => <<"/var/run/docker/netns/f4ed548570c7">>,
            <<"SecondaryIPAddresses">> => null,
            <<"SecondaryIPv6Addresses">> => null},
      <<"Path">> => <<"/docker-entrypoint.sh">>,
      <<"Platform">> => <<"linux">>,
      <<"ProcessLabel">> => <<>>,
      <<"ResolvConfPath">> =>
          <<"/var/lib/docker/containers/7853cf0bbe920586f8e1c73fa9c9da9ffc2751efbca2561a7b9143b6f24e5641/resolv.conf">>,
      <<"RestartCount">> => 0,
      <<"State">> =>
          #{<<"Dead">> => false,
            <<"Error">> => <<>>,
            <<"ExitCode">> => 0,
            <<"FinishedAt">> => <<"0001-01-01T00:00:00Z">>,
            <<"OOMKilled">> => false,
            <<"Paused">> => false,
            <<"Pid">> => 277712,
            <<"Restarting">> => false,
            <<"Running">> => true,
            <<"StartedAt">> => <<"2021-10-17T17:58:08.929303445Z">>,
            <<"Status">> => <<"running">>}}.
