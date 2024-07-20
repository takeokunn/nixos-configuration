# global options
complete -f -c ecspresso -s h -l help -d "Show context-sensitive help."
complete -f -c ecspresso -l envfile -d "environment files"
complete -f -c ecspresso -l debug -d "enable debug log"
complete -f -c ecspresso -l ext-str -d "external string values for Jsonnet"
complete -f -c ecspresso -l ext-code -d "external code values for Jsonnet"
complete -f -c ecspresso -l config -d "config file"

# subcommands
complete -f -c ecspresso -xa appspec -d "output AppSpec YAML for CodeDeploy to STDOUT"
complete -f -c ecspresso -xa delete -d "delete service"
complete -f -c ecspresso -xa deploy -d "deploy service"
complete -f -c ecspresso -xa deregister -d "deregister task definition"
complete -f -c ecspresso -xa diff -d "show diff between task definition, service definition with current running service and task definition"
complete -f -c ecspresso -xa exec -d "execute command on task"
complete -f -c ecspresso -xa init -d "create configuration files from existing ECS service"
complete -f -c ecspresso -xa refresh -d "refresh service. equivalent to deploy --skip-task-definition --force-new-deployment --no-update-service"
complete -f -c ecspresso -xa register -d "register task definition"
complete -f -c ecspresso -xa render -d "render config, service definition or task definition file to STDOUT"
complete -f -c ecspresso -xa revisions -d "show revisions of task definitions"
complete -f -c ecspresso -xa rollback -d "rollback service"
complete -f -c ecspresso -xa run -d "run task"
complete -f -c ecspresso -xa scale -d "scale service. equivalent to deploy --skip-task-definition --no-update-service"
complete -f -c ecspresso -xa status -d "show status of service"
complete -f -c ecspresso -xa tasks -d "list tasks that are in a service or having the same family"
complete -f -c ecspresso -xa verify -d "verify resources in configurations"
complete -f -c ecspresso -xa wait -d "wait until service stable"
complete -f -c ecspresso -xa version -d "show version"

# appspec options
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l task-definition -d "use task definition arn in AppSpec (latest, current or Arn)"
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l update-service -d "update service definition with task definition arn"

# delete options
complete -c ecspresso -n '__fish_seen_subcommand_from delete' -l dry-run -d "dry-run"
complete -c ecspresso -n '__fish_seen_subcommand_from delete' -l force -d "delete without confirmation"
complete -c ecspresso -n '__fish_seen_subcommand_from delete' -l terminate -d "delete with terminate tasks"

# deploy options
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l tasks -d "desired count of tasks"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l skip-task-definition -d "skip register a new task definition"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l force-new-deployment -d "force a new deployment of the service"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l no-wait -d "exit ecspresso immediately after just deployed without waiting for service stable"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l suspend-auto-scaling -d "suspend application auto-scaling attached with the ECS service"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l resume-auto-scaling -d "resume application auto-scaling attached with the ECS service"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l rollback-events -d "roll back when specified events happened (DEPLOYMENT_FAILURE,DEPLOYMENT_STOP_ON_ALARM,DEPLOYMENT_STOP_ON_REQUEST,...) CodeDeploy only."
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l update-service -d "update service attributes by service definition"
complete -c ecspresso -n '__fish_seen_subcommand_from deploy' -l latest-task-definition -d "deploy with the latest task definition without registering a new task definition"

# deregister options
complete -c ecspresso -n '__fish_seen_subcommand_from deregister' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from deregister' -l keeps -d "number of task definitions to keep except in-use"
complete -c ecspresso -n '__fish_seen_subcommand_from deregister' -l revision -d "task definition revision to deregister"
complete -c ecspresso -n '__fish_seen_subcommand_from deregister' -l force -d "force deregister without confirmation"

# diff options
complete -c ecspresso -n '__fish_seen_subcommand_from diff' -l unified -d "unified diff format"

# exec options
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l id -d "task ID"
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l command -d "command to execute"
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l container -d "container name"
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l port-forward -d "enable port forward"
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l local-port -d "local port number"
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l port -d "remote port number (required for --port-forward)"
complete -c ecspresso -n '__fish_seen_subcommand_from exec' -l host -d "remote host (required for --port-forward)"

# init options
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l region -d "AWS region ($AWS_REGION)"
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l cluster -d "ECS cluster name"
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l service -d "ECS service name"
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l task-definition-path -d "path to output task definition file"
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l service-definition-path -d "path to output service definition file"
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l config-file-path
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l force-overwrite -d "overwrite existing files"
complete -c ecspresso -n '__fish_seen_subcommand_from init' -l jsonnet -d "output files as jsonnet format"

# refresh options
complete -c ecspresso -n '__fish_seen_subcommand_from refresh' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from refresh' -l no-wait -d "exit ecspresso immediately after just deployed without waiting for service stable"

# register options
complete -c ecspresso -n '__fish_seen_subcommand_from register' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from register' -l output -d "output the registered task definition as JSON"

# render options
complete -c ecspresso -n '__fish_seen_subcommand_from render' -l jsonnet -d "render as jsonnet format"

# revisions options
complete -c ecspresso -n '__fish_seen_subcommand_from revisions' -l revision -d "revision number to output"
complete -c ecspresso -n '__fish_seen_subcommand_from revisions' -l output -d "output format (json, table, tsv)"

# rollback options
complete -c ecspresso -n '__fish_seen_subcommand_from rollback' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from rollback' -l deregister-task-definition -d "deregister the rolled-back task definition. not works with --no-wait"
complete -c ecspresso -n '__fish_seen_subcommand_from rollback' -l no-wait -d "don\'t wait for the service stable"
complete -c ecspresso -n '__fish_seen_subcommand_from rollback' -l rollback-events -d "roll back when specified events happened (DEPLOYMENT_FAILURE,DEPLOYMENT_STOP_ON_ALARM,DEPLOYMENT_STOP_ON_REQUEST,...) CodeDeploy only."

# run options
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l task-def -d "task definition file for run task"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l no-wait -d "don\'t wait for task to complete"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l overrides -d "task override JSON string"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l overrides-file -d "task override JSON file path"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l skip-task-definition -d "skip register a new task definition"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l count -d "number of tasks to run (max 10)"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l watch-container -d "container name for watching exit code"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l latest-task-definition -d "use the latest task definition without registering a new task definition"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l propagate-tags -d "propagate the tags for the task (SERVICE or TASK_DEFINITION)"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l tags -d "tags for the task: format is KeyFoo=ValueFoo,KeyBar=ValueBar"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l wait-until -d "wait until invoked tasks status reached to (running or stopped)"
complete -c ecspresso -n '__fish_seen_subcommand_from run' -l revision -d "revision of the task definition to run when --skip-task-definition"

# scale options
complete -c ecspresso -n '__fish_seen_subcommand_from scale' -l dry-run -d "dry run"
complete -c ecspresso -n '__fish_seen_subcommand_from scale' -l tasks -d "desired count of tasks"
complete -c ecspresso -n '__fish_seen_subcommand_from scale' -l no-wait -d "exit ecspresso immediately after just deployed without waiting for service stable"
complete -c ecspresso -n '__fish_seen_subcommand_from scale' -l suspend-auto-scaling -d "suspend application auto-scaling attached with the ECS service"
complete -c ecspresso -n '__fish_seen_subcommand_from scale' -l resume-auto-scaling -d "resume application auto-scaling attached with the ECS service"

# status options
complete -c ecspresso -n '__fish_seen_subcommand_from status' -l events -d "show events num"

# tasks options
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l id -d "task ID"
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l output -d "output format"
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l find -d "find a task from tasks list and dump it as JSON"
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l stop -d "stop the task"
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l force -d "stop the task without confirmation"
complete -c ecspresso -n '__fish_seen_subcommand_from appspec' -l trace -d "trace the task"

# verify options
complete -c ecspresso -n '__fish_seen_subcommand_from verify' -l get-secrets -d "get secrets from ParameterStore or SecretsManager"
complete -c ecspresso -n '__fish_seen_subcommand_from verify' -l put-logs -d "put logs to CloudWatchLogs"
