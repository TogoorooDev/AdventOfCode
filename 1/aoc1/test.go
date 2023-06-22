package main

import "fmt"

func splitByNewline(input string) []string {
	var stringArray []string
	var currentString string
	
	for _, char := range input {
		if char == '\n' {
			stringArray = append(stringArray, currentString)
			currentString = ""
		}else {
			currentString = currentString + string(char)
		}
	}

	return stringArray
}

func main(){
	txt := `100
	200
	300

	400
	200
	300`

	split := splitByNewline(txt)

	for _, s := range split {
		fmt.Println(s)
		fmt.Println("a")
	}
}

