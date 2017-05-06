package main

import (
	"os"

	"github.com/urfave/cli"
)

func main() {

	app := cli.NewApp()
	app.Name = "client"
	app.Usage = "Performance tests' client for Erlang"
	app.Action = start
	app.Flags = managerFlags

	app.Run(os.Args)
}
