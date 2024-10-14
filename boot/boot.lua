function table:find(value)
    for k, v in pairs(self) do
        if v == value then
            return k
        end
    end
    return nil
end

function print_table(t, indent)
    indent = indent or ""
    for k, v in pairs(t) do
        if type(v) == "table" then
            print(indent .. tostring(k) .. ":")
            print_table(v, indent .. "  ")
        else
            print(indent .. tostring(k) .. ": " .. tostring(v))
        end
    end
end

------------------------------------------------------------------------------------------------------------------------
--[[ MAKE TOKENS ]]-----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function tokenize(code)

    if not code then
        error("No code to tokenize")
    end

    local WHITESPACE = "%s"
    local OPERATORS = "[%!%+%-%*%/%%^#=]"
    local PUNCTUATION = "[%(%){%}%[%]%;%,%.%:]"
    local IDENTIFIERS = "[%w_]"
    local STRINGS = "[\"']"

    local keywords = {
        "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if",
        "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
    }

    local line = 1
    local column = 0

    local tokens = {}
    local currentToken = ""
    local tokenType = ""
    local i = 1

    while i <= #code do

        local char = code:sub(i, i)

        -- fixme: broken line and column count
        -- count line and column
        if char:gmatch(WHITESPACE) then
            if char == "\n" then
                line = line + 1
                column = 0
            end
            column = column + 1
        end

        if char:match(IDENTIFIERS) then -- Identifiers and keywords
            while i <= #code do
                char = code:sub(i, i)
                if not char:match(IDENTIFIERS) then
                    i = i - 1
                    break
                 end
                currentToken = currentToken .. char
                i = i + 1
            end

            -- check if current token is a keyword
            if table.find(keywords, currentToken) then
                tokenType = "KEYWORD"
            else
                tokenType = "IDENTIFIER"
            end

        elseif char:match(OPERATORS) then -- Operators
            tokenType = "OPERATOR"
            while i <= #code do
                char = code:sub(i, i)
                if not char:match(OPERATORS) then
                    i = i - 1
                    break
                end
                currentToken = currentToken .. char
                i = i + 1
            end
        elseif char:match(PUNCTUATION) then -- Punctuation
            tokenType = "PUNCTUATION"
            currentToken = char
        elseif char:match(STRINGS) then -- Strings
            tokenType = "STRING"
            local quote = char
            currentToken = currentToken .. quote
            i = i + 1
            while i <= #code do
                char = code:sub(i, i)
                currentToken = currentToken .. char
                if char == quote then
                    break
                end
                i = i + 1
            end
        end

        if #currentToken > 0 then
            table.insert(tokens, {
                type = tokenType,
                value = currentToken,
                line = line,
                column = column
            })
            currentToken = ""
        end

        i = i + 1

    end

    if #currentToken > 0 then
        table.insert(tokens, currentToken)
    end

    return tokens
end

------------------------------------------------------------------------------------------------------------------------
--[[ PARSE TOKENS ]]----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function parse(tokens)

    local ast = { body = {} }
    local current_token = 1
    
    local function peek()
        return tokens[current_token]
    end

    local function consume()
        current_token = current_token + 1
        return tokens[current_token - 1]
    end

    local function expect(value)
        local token = consume()
        if token.value ~= value then
            error("Expected " .. value .. " but got " .. token.value)
        end
    end

    local function optional(value)
        local token = peek()
        if token.value == value then
            consume()
            return true
        end
        return false
    end

    local function parse_declaration()
        local declaration = {}

        declaration.name = consume().value
        
        local type
        if peek().value == ":" then
            consume()
            declaration.type = consume().value
        else
            declaration.type = "any"
        end

        return declaration
    end

    local function parse_struct()
        
        -- consume "struct"
        expect("struct")

        -- consume struct name
        local name = consume().value

        -- consume fields
        local fields = {}
        while peek().value ~= "end" do
            table.insert(fields, parse_declaration())
            
            -- if next token is not end, consume comma
            if peek().value ~= "end" then
                expect(",")
            end

        end

        -- consume "end"
        expect("end")

        return {
            type = "struct",
            name = name,
            fields = fields
        }
        
    end

    local function parse_function()
        
        -- consume "function"
        expect("function")

        -- consume function name
        local name = consume().value

        -- consume parameters
        local parameters = {}
        expect("(")
        while peek().value ~= ")" do
            table.insert(parameters, parse_declaration())
            
            -- if next token is not close bracket, consume comma
            if peek().value ~= ")" then
                expect(",")
            end

        end
        expect(")")

        -- consume return type
        local return_type
        if peek().value == ":" then
            consume()
            return_type = consume().value
        else
            return_type = "any"
        end

        expect("end")

        return {
            type = "function",
            name = name,
            parameters = parameters,
            return_type = return_type
        }
        
    end

    local function parse_factor()
        if peek().type == "NUMBER" then
          return consume("NUMBER")
        elseif peek().type == "SYMBOL" and peek().value == "(" then
          consume("SYMBOL", "(")
          local expr = parse_expression()
          consume("SYMBOL", ")")
          return expr
        else
          error("Unexpected token in factor: " .. tostring(peek()))
        end
      end
    
      local function parse_term()
        local left = parse_factor()
        while peek() and peek().type == "OPERATOR" and (peek().value == "*" or peek().value == "/") do
          local operator = consume("OPERATOR").value
          local right = parse_factor()
          left = { type = "BinaryExpression", operator = operator, left = left, right = right }
        end
        return left
      end
    
      local function parse_expression()
        local left = parse_term()
        while peek() and peek().type == "OPERATOR" and (peek().value == "+" or peek().value == "-") do
          local operator = consume("OPERATOR").value
          local right = parse_term()
          left = { type = "BinaryExpression", operator = operator, left = left, right = right }
        end
        return left
      end

    local function parse_return_statement()
        local statement = { type = "return" }
        consume() -- consume "return"
        while peek().value ~= ";" do
            table.insert(statement, parse_expression())
            if peek().value ~= ";" then
                consume() -- consume ","
            end
        end
        consume() -- consume ";"
        return statement
    end

    local function parse_statement()
        local token = peek()
        if token.value == "struct" then
            return parse_struct()
        elseif token.value == "function" then
            return parse_function()
        elseif token.value == "return" then
            return parse_return_statement()
        else 
            error("Unexpected token " .. token.value)
        end
    end 

    while current_token <= #tokens do
        table.insert(ast.body, parse_statement())
    end

    return ast

end

------------------------------------------------------------------------------------------------------------------------
--[[ COMPILE TO LUA ]]--------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function compile(ast)
    local output = ""

    for _, statement in ipairs(ast.body) do
        if statement.type == "function" then
            output = output .. "function " .. statement.name .. "("
            for i, parameter in ipairs(statement.parameters) do
                output = output .. parameter.name
                if i < #statement.parameters then
                    output = output .. ", "
                end
            end
            output = output .. ")\n"
            output = output .. "end\n\n"
        end
    end

    return output
end

------------------------------------------------------------------------------------------------------------------------
--[[ RUN ]]-------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- filename from arg
local filename = arg[1]

-- read file to string
local file = io.open(filename, "r")
local script = file:read("*a")
file:close()

-- tokenize file
local tokens = tokenize(script)
--print_table(tokens)

-- parse tokens
local ast = parse(tokens)
print_table(ast)

-- write file
local file = io.open("build/output.lua", "w")
--file:write(compile(ast))
file:close()