package main

import (
	"errors"
	"log"
	"time"

	"github.com/urfave/cli"
)

var managerFlags = []cli.Flag{
	cli.IntFlag{
		Name:  "number, n",
		Value: 2,
	},
}

type manager struct {
	*cli.Context
}

func newManager(c *cli.Context) *manager {
	return &manager{Context: c}
}

func (m *manager) start() error {
	n := m.NArg()
	if n <= 0 {
		err := errors.New("no agent address")
		log.Fatal(err)
		return err
	}

	agentAddr := m.Args().Get(0)

	botNumber := m.Int("number")
	for i := 0; i < botNumber; i++ {
		go startBot(m.Context, agentAddr)
	}
	time.Sleep(10 * time.Second)
	return nil
}

func start(c *cli.Context) error {
	m := newManager(c)
	return m.start()
}
