package main

import (
	"bytes"
	"net"
	"time"

	"github.com/urfave/cli"
)

type bot struct {
	*cli.Context
	net.Conn
}

func newBot(c *cli.Context) *bot {
	return &bot{Context: c}
}

func startBot(c *cli.Context, address string) error {
	bot := newBot(c)
	return bot.start(address)
}

func (b *bot) start(address string) error {
	conn, err := net.Dial("tcp", address)
	if err != nil {
		return err
	}
	b.Conn = conn

	buff := bytes.NewBufferString("echo\r\n").Bytes()

	for err = nil; err == nil; {
		_, err = b.Conn.Write(buff)
		time.Sleep(time.Millisecond)
	}

	return err
}
