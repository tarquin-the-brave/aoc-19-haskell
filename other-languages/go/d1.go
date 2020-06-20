#!/usr/bin/env gorun
package main

import (
  "fmt"
  "io/ioutil"
  "strings"
  "strconv"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func tot(f func(int) int) func([]int) int {
  return func(xs []int) int {
    tot := 0
    for _, x := range xs {
      tot += f(x)
    }
    return tot
  }
}

func map_to_int(ss []string) []int {
  var out []int
  for _, s := range ss {
    x, err := strconv.Atoi(s)
    check(err)
    out = append(out, x)
  }
  return out
}

func part2(x int) int {
  res := x / 3 - 2
  if res < 0 { return 0 }
  return res + part2(res)
}

func main() {
  contents, err := ioutil.ReadFile("../../day1-inputs.txt")
  check(err)

  data := map_to_int(strings.Split(strings.TrimSpace(string(contents)), "\n"))

  // Part 1
  fmt.Println(tot(func(x int) int { return x / 3 - 2})(data))

  // Part 2
  fmt.Println(tot(part2)(data))
}

