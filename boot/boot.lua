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

local keywords = {
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if",
    "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
}

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
    local INTEGER = "%d"
    local BOOLEAN = "true|false"
    local STRINGS = "[\"']"

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

        if char:match(INTEGER) then -- integer or float
            tokenType = "INTEGER"
            while i <= #code do
                char = code:sub(i, i)
                if char == "." then
                    tokenType = "FLOAT"
                elseif not char:match(INTEGER) then
                    i = i - 1
                    break
                end
                currentToken = currentToken .. char
                i = i + 1
            end
        elseif char:match(BOOLEAN) then -- boolean
            tokenType = "BOOLEAN"
            while i <= #code do
                char = code:sub(i, i)
                if not char:match(BOOLEAN) then
                    i = i - 1
                    break
                end
                currentToken = currentToken .. char
                i = i + 1
            end
        elseif char:match(IDENTIFIERS) then -- Identifiers and keywords
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

    -- insert EOF
    table.insert(tokens, { type = "EOF" })

    return tokens
end

------------------------------------------------------------------------------------------------------------------------
--[[ PARSE TOKENS ]]----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function parse(tokens)

    local ast = { body = {} }
    local current_token = 1

    -- header
    local parse_expression
    
    local peek = function()
        return tokens[current_token]
    end

    local consume = function()
        current_token = current_token + 1
        return tokens[current_token - 1]
    end

    local expect = function(value)
        local token = consume()
        if token.value ~= value then
            error("Expected " .. value .. " but got " .. token.value .. " at line " .. token.line .. " column " .. token.column)
        end
    end

    local optional = function(value)
        local token = peek()
        if token.value == value then
            consume()
            return true
        end
        return false
    end

    local parse_declaration = function()
        local declaration = {}

        declaration.name = consume().value
        
        local type
        if peek().value == ":" then
            consume()
            declaration.type = consume().value
        else
            declaration.type = "any"
        end

        -- parse optional assignmen
        if optional("=") then
            declaration.value = parse_expression()
        end

        return declaration
    end

    local parse_struct = function()
        
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

    local parse_function_call = function()

        -- consume function name
        local name = consume().value

        -- consume arguments
        local arguments = {}
        expect("(")
        while peek().value ~= ")" do
            table.insert(arguments, parse_expression())
            
            -- if next token is not close bracket, consume comma
            if peek().value ~= ")" then
                expect(",")
            end

        end
        expect(")")

        return {
            type = "function_call",
            name = name,
            arguments = arguments
        }

    end 

    local parse_primary_expression = function()
        local token = peek()
        if token.type == "IDENTIFIER" then

            -- if next token is "(", parse function call
            if tokens[current_token + 1].value == "(" then
                return parse_function_call()
            end

            return { type = "identifier", value = consume().value }
        elseif token.type == "STRING" then
            return { type = "string", value = consume().value }
        elseif token.type == "INTEGER" then
            return { type = "integer", value = consume().value }
        elseif token.type == "FLOAT" then
            return { type = "float", value = consume().value }
        elseif token.value == "true" or token.value == "false" then
            return { type = "boolean", value = consume().value }
        elseif token.value == "(" then
            consume() -- consume "("
            local expression = parse_expression()
            expect(")")
            return expression
        elseif token.type == "EOF" or token.value == "end" then
            return {}
        else
            error("Unexpected token " .. (token.type or "UNKNOWN") .. " " .. (token.value or "UNKNOWN"))
        end
    end

    local parse_unary_expression = function()
        local token = peek()
        if token.value == "-" or token.value == "not" then
            consume()
            return { type = "unary", operator = token.value, expression = parse_unary_expression() }
        else
            return parse_primary_expression()
        end
    end

    local parse_multiplicative_expression = function()
        local expression = parse_unary_expression()
        while peek().value == "*" or peek().value == "/" do
            local token = consume()
            expression = { type = "binary", operator = token.value, left = expression, right = parse_unary_expression() }
        end
        return expression
    end

    local parse_additive_expression = function()
        local expression = parse_multiplicative_expression()
        while peek().value == "+" or peek().value == "-" do
            local token = consume()
            expression = { type = "binary", operator = token.value, left = expression, right = parse_multiplicative_expression() }
        end
        return expression
    end

    local parse_relational_expression = function()
        local expression = parse_additive_expression()
        while peek().value == "==" or peek().value == "!=" or peek().value == "<" or peek().value == ">" or peek().value == "<=" or peek().value == ">=" do
            local token = consume()
            expression = { type = "binary", operator = token.value, left = expression, right = parse_additive_expression() }
        end
        return expression
    end 

    local parse_equality_expression = function()
        local expression = parse_relational_expression()
        while peek().value == "==" or peek().value == "!=" do
            local token = consume()
            expression = { type = "binary", operator = token.value, left = expression, right = parse_relational_expression() }
        end
        return expression
    end 

    local parse_logical_and_expression = function()
        local expression = parse_equality_expression()
        while peek().value == "and" do
            local token = consume()
            expression = { type = "binary", operator = token.value, left = expression, right = parse_equality_expression() }
        end
        return expression
    end

    local parse_logical_or_expression = function()
        local expression = parse_logical_and_expression()
        while peek().value == "or" do
            local token = consume()
            expression = { type = "binary", operator = token.value, left = expression, right = parse_logical_and_expression() }
        end
        return expression
    end

    local function precedence(operator)
        if operator == "+" or operator == "-" then
          return 1
        elseif operator == "*" or operator == "/" then
          return 2
        elseif operator == "^" then
          return 3
        else
          return 0  -- For parentheses and other tokens
        end
    end

    

    parse_expression = function()
        return shunting_yard()
    end

    local parse_return_statement = function()
        expect("return")
        local expr = parse_expression()
        return {
            type = "return",
            expression = expr
        }
    end

    local parse_variable_declaration = function()
        expect("let")
        local declaration = parse_declaration()
        return {
            type = "variable_declaration",
            declaration = declaration
        }
    end

    local parse_function = function()
    end

    local parse_if_statement = function()

        -- consume "if"
        expect("if")

        -- consume condition
        local condition = parse_expression()

        -- consume "then"
        expect("then")

        -- consume statements
        local statements = {}
        while peek().value ~= "end" do
            table.insert(statements, parse_statement())
        end

        -- consume "end"
        expect("end")

        return {
            type = "if",
            condition = condition,
            statements = statements
        }
    end

    local parse_statement = function()
        local token = peek()
        if token.value == "struct" then
            return parse_struct()
        elseif token.value == "function" then
            return parse_function()
        elseif token.value == "return" then
            return parse_return_statement()
        elseif token.value == "var" or token.value == "local" or token.value == "const" then
            return parse_variable_declaration()
        elseif token.value == "if" then
            return parse_if_statement()
        elseif token.type == "IDENTIFIER" then
            
            -- if next token is "(", parse function call
            if tokens[current_token + 1].value == "(" then
                return parse_function_call()

            -- if next token is "=", parse assignment
            elseif tokens[current_token + 1].value == "=" then
                return parse_assignment()
            end

        else 
            error("Unexpected token " .. token.value .. " at line " .. token.line .. " column " .. token.column)
        end
    end 

    parse_function = function()
        
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

        -- parse statements
        local statements = {}
        while peek().value ~= "end" do
            table.insert(statements, parse_statement())
        end

        expect("end")

        return {
            type = "function",
            name = name,
            parameters = parameters,
            statements = statements,
            return_type = return_type
        }
        
    end

    -- loop until EOF
    while peek().type ~= "EOF" do
        table.insert(ast.body, parse_statement())
    end

    return ast

end

------------------------------------------------------------------------------------------------------------------------
--[[ COMPILE TO LUA ]]--------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function compile(ast)
    local output = ""

    -- unwrap expression from the ast into a lua expression
    local unwrap_expression
    unwrap_expression = function(expr)
        if expr.type == "identifier" then
            return expr.value
        elseif expr.type == "function_call" then
            local output = expr.name .. "("
            for i, argument in ipairs(expr.arguments) do
                output = output .. unwrap_expression(argument)
                if i < #expr.arguments then
                    output = output .. ", "
                end
            end
            return output .. ")"
        elseif expr.type == "string" then
            return expr.value
        elseif expr.type == "integer" then
            return expr.value
        elseif expr.type == "float" then
            return expr.value
        elseif expr.type == "boolean" then
            return expr.value
        elseif expr.type == "binary" then
            return "(" .. unwrap_expression(expr.left) .. " " .. expr.operator .. " " .. unwrap_expression(expr.right) .. ")"
        elseif expr.type == "unary" then
            return expr.operator .. unwrap_expression(expr.expression)
        end
    end

    local compile_statement
    compile_statement = function(statement)
        local output = ""

        print("Statement: " .. statement.type)  

        if statement.type == "return" then
            output = output .. "return " .. unwrap_expression(statement.expression) .. "\n"
        elseif statement.type == "variable_declaration" then
            output = output .. "local " .. statement.declaration.name
            if statement.declaration.value then
                output = output .. " = " .. unwrap_expression(statement.declaration.value)
            end
            output = output .. "\n"
        elseif statement.type == "function_call" then
            output = output .. statement.name .. "("
            for i, argument in ipairs(statement.arguments) do
                output = output .. unwrap_expression(argument)
                if i < #statement.arguments then
                    output = output .. ", "
                end
            end
            output = output .. ")\n"
        end

        return output
    end

    local indent = 0
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

            print("Function: " .. statement.name .. " with " .. #statement.parameters .. " parameters" .. " and " .. #statement.statements .. " statements")

            -- print statements
            indent = indent + 1
            for _, stmt in ipairs(statement.statements) do
                output = output .. compile_statement(stmt)
            end

            output = output .. "end\n\n"
        else 
            output = output .. compile_statement(statement)
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
--print("Tokens:")
--print_table(tokens)

-- parse tokens
local ast = parse(tokens)
print("AST:")
print_table(ast)

-- write file
if ast then
    local file = io.open("build/output.lua", "w")
    file:write(compile(ast))
    file:close()
end