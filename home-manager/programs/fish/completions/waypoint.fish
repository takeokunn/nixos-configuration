# global option
complete -f -c waypoint -s h -l help

# subcommand
complete -f -c waypoint -n '__fish_use_subcommand' -xa login -d "Log in to a Waypoint server"
complete -f -c waypoint -n '__fish_use_subcommand' -xa build -d "Build a new versioned artifact from source"
complete -f -c waypoint -n '__fish_use_subcommand' -xa deploy -d "Deploy a pushed artifact"
complete -f -c waypoint -n '__fish_use_subcommand' -xa release -d "Release a deployment"
complete -f -c waypoint -n '__fish_use_subcommand' -xa status -d "List and refresh application statuses."
complete -f -c waypoint -n '__fish_use_subcommand' -xa up -d "Perform the build, deploy, and release steps"
complete -f -c waypoint -n '__fish_use_subcommand' -xa artifact -d "Artifact and build management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa auth-method -d "Auth method management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa config -d "Application configuration management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa context -d "Server access configurations"
complete -f -c waypoint -n '__fish_use_subcommand' -xa deployment -d "Deployment creation and management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa destroy -d "Delete all the resources created"
complete -f -c waypoint -n '__fish_use_subcommand' -xa docs -d "Show documentation for components"
complete -f -c waypoint -n '__fish_use_subcommand' -xa exec -d "Execute a command in the context of a running application instance"
complete -f -c waypoint -n '__fish_use_subcommand' -xa fmt -d "Rewrite waypoint.hcl configuration to a canonical format"
complete -f -c waypoint -n '__fish_use_subcommand' -xa hostname -d "Application URLs"
complete -f -c waypoint -n '__fish_use_subcommand' -xa init -d "Initialize and validate a project"
complete -f -c waypoint -n '__fish_use_subcommand' -xa install -d "Install the Waypoint server to Kubernetes, Nomad, ECS, or Docker"
complete -f -c waypoint -n '__fish_use_subcommand' -xa job -d "Job introspection and management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa logs -d "Show log output from an application's current deployment"
complete -f -c waypoint -n '__fish_use_subcommand' -xa pipeline -d "Pipeline management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa project -d "Project management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa runner -d "Runner management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa server -d "Server management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa task -d "Task introspection and management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa trigger -d "Trigger URL management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa ui -d "Open the web UI"
complete -f -c waypoint -n '__fish_use_subcommand' -xa user -d "User information and management"
complete -f -c waypoint -n '__fish_use_subcommand' -xa version -d "Prints the version of this Waypoint CLI"
complete -f -c waypoint -n '__fish_use_subcommand' -xa workspace -d "Manage workspaces"

# login option
complete -c waypoint -n '__fish_seen_subcommand_from login' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from login' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from login' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from login' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from login' -l server-addr
complete -c waypoint -n '__fish_seen_subcommand_from login' -l server-tls
complete -c waypoint -n '__fish_seen_subcommand_from login' -l server-tls-skip-verify
complete -c waypoint -n '__fish_seen_subcommand_from login' -l auth-method
complete -c waypoint -n '__fish_seen_subcommand_from login' -l from-kubernetes
complete -c waypoint -n '__fish_seen_subcommand_from login' -l from-kubernetes-namespace
complete -c waypoint -n '__fish_seen_subcommand_from login' -l from-kubernetes-secret
complete -c waypoint -n '__fish_seen_subcommand_from login' -l from-kubernetes-service
complete -c waypoint -n '__fish_seen_subcommand_from login' -l token

# build option
complete -c waypoint -n '__fish_seen_subcommand_from build' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from build' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from build' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from build' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from build' -l label
complete -c waypoint -n '__fish_seen_subcommand_from build' -l local
complete -c waypoint -n '__fish_seen_subcommand_from build' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from build' -l var
complete -c waypoint -n '__fish_seen_subcommand_from build' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from build' -l push

# deploy option
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l label
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l local
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l var
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l prune
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l prune-retain
complete -c waypoint -n '__fish_seen_subcommand_from deploy' -l release

# release subcommand
complete -c waypoint -n '__fish_seen_subcommand_from release' -xa list

# release option
complete -c waypoint -n '__fish_seen_subcommand_from release' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from release' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from release' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from release' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from release' -l label
complete -c waypoint -n '__fish_seen_subcommand_from release' -l local
complete -c waypoint -n '__fish_seen_subcommand_from release' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from release' -l var
complete -c waypoint -n '__fish_seen_subcommand_from release' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from release' -l deployment
complete -c waypoint -n '__fish_seen_subcommand_from release' -l prune
complete -c waypoint -n '__fish_seen_subcommand_from release' -l prune-retain
complete -c waypoint -n '__fish_seen_subcommand_from release' -l repeat

# status
complete -c waypoint -n '__fish_seen_subcommand_from status' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from status' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from status' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from status' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from status' -l label
complete -c waypoint -n '__fish_seen_subcommand_from status' -l local
complete -c waypoint -n '__fish_seen_subcommand_from status' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from status' -l var
complete -c waypoint -n '__fish_seen_subcommand_from status' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from status' -l all-projects
complete -c waypoint -n '__fish_seen_subcommand_from status' -l json
complete -c waypoint -n '__fish_seen_subcommand_from status' -l refresh
complete -c waypoint -n '__fish_seen_subcommand_from status' -l verbose

# up option
complete -c waypoint -n '__fish_seen_subcommand_from up' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from up' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from up' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from up' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from up' -l label
complete -c waypoint -n '__fish_seen_subcommand_from up' -l local
complete -c waypoint -n '__fish_seen_subcommand_from up' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from up' -l var
complete -c waypoint -n '__fish_seen_subcommand_from up' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from up' -l prune
complete -c waypoint -n '__fish_seen_subcommand_from up' -l prune-retain

# artifact subcommand
complete -c waypoint -n '__fish_seen_subcommand_from artifact' -xa build -d "Build a new versioned artifact from source"
complete -c waypoint -n '__fish_seen_subcommand_from artifact' -xa list -d "List pushed artifacts."
complete -c waypoint -n '__fish_seen_subcommand_from artifact' -xa list-builds -d "List builds."
complete -c waypoint -n '__fish_seen_subcommand_from artifact' -xa push -d "Push a build's artifact to a registry"

# auth-method subcommand
complete -c waypoint -n '__fish_seen_subcommand_from auth-method' -xa delete -d "Delete a previously configured auth method."
complete -c waypoint -n '__fish_seen_subcommand_from auth-method' -xa inspect -d "Show detailed information about a configured auth method"
complete -c waypoint -n '__fish_seen_subcommand_from auth-method' -xa list -d "List all configured auth methods"
complete -c waypoint -n '__fish_seen_subcommand_from auth-method' -xa set -d "Create or update an auth method"

# config subcommand
complete -c waypoint -n '__fish_seen_subcommand_from config' -xa get -d "Get config variables."
complete -c waypoint -n '__fish_seen_subcommand_from config' -xa set -d "Set a config variable."
complete -c waypoint -n '__fish_seen_subcommand_from config' -xa source-get -d "Get the configuration for a dynamic source plugin"
complete -c waypoint -n '__fish_seen_subcommand_from config' -xa source-set -d "Set the configuration for a dynamic source plugin"
complete -c waypoint -n '__fish_seen_subcommand_from config' -xa sync -d "Synchronize declared variables and pipeline configs in a waypoint.hcl"

# context subcommand
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa clear -d "Unset the default context."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa create -d "Create a context."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa delete -d "Output context info."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa inspect -d "Output context info."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa list -d "List contexts."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa rename -d "Rename a context."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa set -d "Set a property of the current context."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa use -d "Set the default context."
complete -c waypoint -n '__fish_seen_subcommand_from context' -xa verify -d "Verify server access with a context"

# deployment subcommand
complete -c waypoint -n '__fish_seen_subcommand_from deployment' -xa deploy -d "Deploy a pushed artifact"
complete -c waypoint -n '__fish_seen_subcommand_from deployment' -xa destroy -d "Destroy one or more deployments."
complete -c waypoint -n '__fish_seen_subcommand_from deployment' -xa list -d "List deployments."

# destroy option
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l label
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l local
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l var
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from destroy' -l auto-approve

# docs optioin
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l label
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l local
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l var
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l builtin
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l json
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l markdown
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l plugin
complete -c waypoint -n '__fish_seen_subcommand_from docs' -l type

# exec option
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l label
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l local
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l var
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from exec' -l instance

# fmt option
complete -c waypoint -n '__fish_seen_subcommand_from fmt' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from fmt' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from fmt' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from fmt' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from fmt' -l check
complete -c waypoint -n '__fish_seen_subcommand_from fmt' -l write

# hostname subcommand
complete -c waypoint -n '__fish_seen_subcommand_from hostname' -xa delete -d "Delete a previously registered hostname."
complete -c waypoint -n '__fish_seen_subcommand_from hostname' -xa list -d "List all registered hostnames."
complete -c waypoint -n '__fish_seen_subcommand_from hostname' -xa register -d "Register a hostname to route to your apps."

# init option
complete -c waypoint -n '__fish_seen_subcommand_from init' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from init' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from init' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from init' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from init' -l from-project
complete -c waypoint -n '__fish_seen_subcommand_from init' -l into
complete -c waypoint -n '__fish_seen_subcommand_from init' -l update

# install option
complete -c waypoint -n '__fish_seen_subcommand_from install' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from install' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from install' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from install' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from install' -l accept-tos
complete -c waypoint -n '__fish_seen_subcommand_from install' -l context-create
complete -c waypoint -n '__fish_seen_subcommand_from install' -l context-set-default
complete -c waypoint -n '__fish_seen_subcommand_from install' -l platform
complete -c waypoint -n '__fish_seen_subcommand_from install' -l docker-odr-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l docker-runner-socket-path
complete -c waypoint -n '__fish_seen_subcommand_from install' -l docker-server-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-cluster
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-cpu
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-execution-role-name
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-mem
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-odr-cpu
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-odr-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-odr-mem
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-region
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-server-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-subnets
complete -c waypoint -n '__fish_seen_subcommand_from install' -l ecs-task-role-name
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-advertise-internal
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-annotate-service
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-config-path
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-context
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-cpu-limit
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-cpu-request
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-helm-versionk8s-mem-limit
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-mem-request
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-namespace
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-odr-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-pull-policy
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-pull-secret
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-runner-service-account
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-runner-service-account-init
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-secret-file
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-server-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-storage-request
complete -c waypoint -n '__fish_seen_subcommand_from install' -l k8s-storageclassname
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-annotate-service
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-auth-soft-fail
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-datacenter
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-domain
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-service
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-service-backend-tags
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-service-hostname
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-service-ui-tags
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-consul-token
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-external-id
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-fs
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-mount-flags
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-parameters
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-plugin-id
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-secrets
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-topologies
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-volume
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-volume-capacity-max
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-volume-capacity-min
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-csi-volume-provider
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-dc
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-host
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-host-volume
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-namespace
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-odr-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-policy-override
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-region
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-cpu
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-csi-volume
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-csi-volume-capacity-max
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-csi-volume-capacity-min
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-csi-volume-provider
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-host-volume
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-runner-memory
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-server-cpu
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-server-image
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-server-memory
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-service-backend-tags
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-service-provider
complete -c waypoint -n '__fish_seen_subcommand_from install' -l nomad-service-ui-tags

# job option
complete -c waypoint -n '__fish_seen_subcommand_from job' -xa cancel -d "Cancel a running a job by id"
complete -c waypoint -n '__fish_seen_subcommand_from job' -xa get-stream -d "Attach a local CLI to a job stream by id"
complete -c waypoint -n '__fish_seen_subcommand_from job' -xa inspect -d "Inspect the details of a job by id in Waypoint"
complete -c waypoint -n '__fish_seen_subcommand_from job' -xa list -d "List all jobs in Waypoint"

# logs option
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l label
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l local
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l remote-source
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l var
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l var-file
complete -c waypoint -n '__fish_seen_subcommand_from logs' -l deployment-seq

# pipeline subcommand
complete -c waypoint -n '__fish_seen_subcommand_from pipeline' -xa inspect -d "Inspect the full details of a pipeline by id"
complete -c waypoint -n '__fish_seen_subcommand_from pipeline' -xa list -d "List all pipelines for a project."
complete -c waypoint -n '__fish_seen_subcommand_from pipeline' -xa run -d "Manually execute a pipeline by name."

# project subcommand
complete -c waypoint -n '__fish_seen_subcommand_from projet' -xa apply -d "Create or update a project."
complete -c waypoint -n '__fish_seen_subcommand_from projet' -xa destroy -d "Delete the specified project and optionally destroy its resources."
complete -c waypoint -n '__fish_seen_subcommand_from projet' -xa inspect -d "Inspect the details of a project."
complete -c waypoint -n '__fish_seen_subcommand_from projet' -xa list -d "List all registered projects."

# runner subcommand
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa adopt -d "Adopt a pending runner"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa agent -d "Run a runner for executing remote operations."
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa forget -d "Forget a previously registered runner"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa inspect -d "Show detailed information about a runner."
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa install -d "Install a Waypoint runner to Kubernetes, Nomad, ECS, or Docker"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa list -d "List registered runners"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa profile -d "Manage runner profiles"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa reject -d "Reject a pending runner"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa token -d "Request a new token to run a runner"
complete -c waypoint -n '__fish_seen_subcommand_from runner' -xa uninstall -d "Uninstall a Waypoint runner from Kubernetes, Nomad, ECS, or Docker"

# server subcommand
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa bootstrap -d "Bootstrap the server and retrieve the initial auth token"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa config-set -d "Set the server online configuration"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa cookie -d "Output server cookie value"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa install -d "Install the Waypoint server to Kubernetes, Nomad, ECS, or Docker"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa restore -d "Stage a snapshot on the current server for data restoration"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa run -d "Manually run the builtin server"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa snapshot -d "Write a backup of the server data"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa uninstall -d "Uninstall the Waypoint server"
complete -c waypoint -n '__fish_seen_subcommand_from server' -xa upgrade -d "Upgrades Waypoint server in the current context to the latest version"

# task subcommand
complete -c waypoint -n '__fish_seen_subcommand_from task' -xa cancel -d "Cancel an On-Demand Runner Task running in Waypoint"
complete -c waypoint -n '__fish_seen_subcommand_from task' -xa inspect -d "Inspect an On-Demand Runner Task from Waypoint"
complete -c waypoint -n '__fish_seen_subcommand_from task' -xa list -d "List all On-Demand Runner Tasks in Waypoint"

# trigger subcommand
complete -c waypoint -n '__fish_seen_subcommand_from trigger' -xa create -d "Create and register a trigger URL."
complete -c waypoint -n '__fish_seen_subcommand_from trigger' -xa delete -d "Delete a registered trigger URL."
complete -c waypoint -n '__fish_seen_subcommand_from trigger' -xa inspect -d "Inspect a registered trigger URL configuration."
complete -c waypoint -n '__fish_seen_subcommand_from trigger' -xa list -d "List registered trigger URL configurations."
complete -c waypoint -n '__fish_seen_subcommand_from trigger' -xa update -d "Update a registered trigger URL."

# ui option
complete -c waypoint -n '__fish_seen_subcommand_from ui' -l app -s a
complete -c waypoint -n '__fish_seen_subcommand_from ui' -l plain
complete -c waypoint -n '__fish_seen_subcommand_from ui' -l project -s p
complete -c waypoint -n '__fish_seen_subcommand_from ui' -l workspace -s w
complete -c waypoint -n '__fish_seen_subcommand_from ui' -l authenticate

# user subcommand
complete -c waypoint -n '__fish_seen_subcommand_from user' -xa inspect -d "Show details about a single user"
complete -c waypoint -n '__fish_seen_subcommand_from user' -xa invite -d "Invite a user to join the Waypoint server"
complete -c waypoint -n '__fish_seen_subcommand_from user' -xa modify -d "Modify details about a user"
complete -c waypoint -n '__fish_seen_subcommand_from user' -xa token -d "Request a new token to access the server"

# workspace subcommand
complete -c waypoint -n '__fish_seen_subcommand_from workspace' -xa create -d "Create a workspace with a given name."
complete -c waypoint -n '__fish_seen_subcommand_from workspace' -xa inspect -d "Output information for a given Workspace."
complete -c waypoint -n '__fish_seen_subcommand_from workspace' -xa list -d "List workspaces for the current context."
