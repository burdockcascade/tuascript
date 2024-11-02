function table:find(value)
    for k, v in pairs(self) do
        if v == value then
            return k
        end
    end
    return nil
end

function pretty_table(t, indent)
    local output = ""
    indent = indent or ""
    for k, v in pairs(t) do
        if type(v) == "table" then
            output = output .. indent .. tostring(k) .. ":" .. "\n"
            output = output .. pretty_table(v, indent .. "  ")
        else
            output = output .. indent .. tostring(k) .. ": " .. tostring(v) .. "\n"
        end
    end
    return output
end

function table_to_string(tbl, indent_level)
    indent_level = indent_level or 0  -- Default indentation level is 0
    local indent = string.rep("  ", indent_level) -- Create indent string using repetition
    local result = "{\n"

    for k, v in pairs(tbl) do
      result = result .. indent .. "  "  -- Add two spaces for indentation

      if type(k) == "number" then
        result = result .. "[" .. k .. "] = "
      elseif type(k) == "string" then
        result = result .. '["' .. k .. '"] = '
      end
  
      if type(v) == "number" then
        result = result .. v .. ",\n"
      elseif type(v) == "string" then
        result = result .. '"' .. v .. '",\n'
      elseif type(v) == "table" then
        result = result .. table_to_string(v, indent_level + 1) .. ",\n"
      else
        result = result .. tostring(v) .. ",\n"
      end
    end
    result = result .. indent .. "}"  -- Remove trailing comma and space, add closing brace and indent
    return result
  end

function print_table(t)
    print(pretty_table(t))
end

local keywords = {
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if",
    "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
}

------------------------------------------------------------------------------------------------------------------------
--[[ MAKE TOKENS ]]-----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function tokenize(code)

    if not code then
        error("No code to tokenize")
    end

    print("Tokenizing code")

    local WHITESPACE = "%s"
    local OPERATORS = "[%!%+%-%*%/%>%<%%^#=]"
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

    if not tokens then
        error("No tokens to parse")
    end

    print("Parsing tokens")

    local ast = { statements = {} }
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
            return { type = "unary", operator = token.value, expression = parse_primary_expression() }
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
        while peek().value == "<" or peek().value == ">" or peek().value == "<=" or peek().value == ">=" do
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

    parse_expression = function()

        -- if expression is an array with square brackets
        if peek().value == "[" then
            consume()
            local array = {}

            while peek().value ~= "]" do
                table.insert(array, parse_expression())
                if peek().value ~= "]" then
                    expect(",")
                end
            end
            expect("]")
            return {
                type = "array",
                value = array
            }
        elseif peek().value == "{" then
            consume()
            local table = {}

            while peek().value ~= "}" do
                local key = parse_expression()
                expect(":")
                local value = parse_expression()
                table[key] = value
                if peek().value ~= "}" then
                    expect(",")
                end
            end
            expect("}")
            return {
                type = "table",
                value = table
            }
        else 
            return parse_logical_or_expression()
        end

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
        expect("var")
        local declaration = parse_declaration()
        return {
            type = "variable_declaration",
            declaration = declaration
        }
    end

    local parse_function = function()
    end

    local parse_statement

    local parse_if_block = function()

        local cases = {}
        local elseblock = {}

        -- consume "if" blocks
        while peek().value == "if" do
            consume()
            local condition = parse_expression()
            expect("then")
            local statements = {}
            while peek().value ~= "end" and peek().value ~= "else" do
                table.insert(statements, parse_statement())
            end

            consume() 

            table.insert(cases, { condition = condition, statements = statements })
        end

        -- if last token is "else"
        current_token = current_token - 1
        if peek().value == "else" then
            consume()
            while peek().value ~= "end" do
                table.insert(elseblock, parse_statement())
            end
            expect("end")
        else
            expect("end")
        end

        local block = {
            type = "if",
            cases = cases,
            elseblock = elseblock
        }

        return block
        
    end

    local parse_assignment = function()
        local name = consume().value
        expect("=")
        local expression = parse_expression()
        return {
            type = "assignment",
            name = name,
            expression = expression
        }
    end

    local parse_enum = function()
        expect("enum")
        local name = consume().value
        local items = {}
        while peek().value ~= "end" do
            table.insert(items, consume().value)
            if peek().value ~= "end" then
                expect(",")
            end
        end
        expect("end")
        return {
            type = "enum",
            name = name,
            items = items
        }
    end

    local parse_class = function()
        expect("class")

        local name = consume().value

        print("Parsing class " .. name)
    
        -- consume functions
        local functions = {}
        local fields = {}
        while peek().value ~= "end" do

            if optional("var") then
                local declaration = parse_declaration()
                table.insert(fields, declaration)
            end

            -- consume function
            if optional("function") then
                table.insert(functions, parse_function(name))
            end
        end

        expect("end")

        return {
            type = "class",
            name = name,
            fields = fields,
            functions = functions
        }

    end

    parse_identifier_chain = function()
        local chain = {}

        while peek().type == "IDENTIFIER" do
            table.insert(chain, consume().value)
            if peek().value == "." then
                consume()
            else
                break
            end
        end

        return chain
        
    end

    parse_statement = function()
        local token = peek()
        if token.value == "struct" then
            return parse_struct()
        elseif token.value == "class" then
            return parse_class()
        elseif token.value == "enum" then
            return parse_enum()
        elseif token.value == "function" then
            return parse_function()
        elseif token.value == "return" then
            return parse_return_statement()
        elseif token.value == "var" then
            return parse_variable_declaration()
        elseif token.value == "if" then
            return parse_if_block()
        elseif token.type == "IDENTIFIER" then
            
            local chain = parse_identifier_chain()

            --if tokens[current_token + 1].value == "=" then
                --return parse_assignment()
            --end

            return {
                type = "identifier_chain",
                chain = chain
            }
        end
    end 

    parse_function = function(class)
        
        -- consume "function"
        optional("function")

        -- consume function name
        local name = consume().value

        print("Parsing function " .. name)

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
        if optional(":") then
            return_type = consume().value
        else
            return_type = "any"
        end

        -- parse statements
        local statements = {}
        while peek().value ~= "end" do
            local statement = parse_statement()
            if statement then
                table.insert(statements, statement)
            end
        end

        expect("end")

        return {
            type = "function",
            class = class or nil,
            name = name,
            parameters = parameters,
            statements = statements,
            return_type = return_type
        }
        
    end

    -- loop until EOF
    while peek().type ~= "EOF" do
        local s =  parse_statement(ast)
        if s then
            table.insert(ast.statements, s)
        end
    end

    return ast

end

------------------------------------------------------------------------------------------------------------------------
--[[ COMPILE TO LUA ]]--------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

function compile(ast)

    if not ast then
        error("No ast to compile")
    end

    print("Compiling to lua")

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
            return expr.operator .. " " .. unwrap_expression(expr.expression)
        elseif expr.type == "array" then
            local output = "{"
            for i, value in ipairs(expr.value) do
                output = output .. unwrap_expression(value)
                if i < #expr.value then
                    output = output .. ", "
                end
            end
            return output .. "}"
        elseif expr.type == "table" then
            local output = "{"
            for key, value in pairs(expr.value) do
                output = output .. "[" .. unwrap_expression(key) .. "] = " .. unwrap_expression(value)
            end
            return output .. "}"
        else
            return "nil"
        end
    end

    local compile_statement
    compile_statement = function(statement)
        local output = ""

        if statement.type == "return" then
            output = output .. "return " .. unwrap_expression(statement.expression) .. "\n"
        elseif statement.type == "variable_declaration" then
            output = output .. "local " .. statement.declaration.name
            if statement.declaration.value then
                output = output .. " = " .. unwrap_expression(statement.declaration.value)
            end
            output = output .. "\n"
        elseif statement.type == "assignment" then
            output = output .. statement.name .. " = " .. unwrap_expression(statement.expression) .. "\n"
        elseif statement.type == "function_call" then
            output = output .. statement.name .. "("
            for i, argument in ipairs(statement.arguments) do
                output = output .. unwrap_expression(argument)
                if i < #statement.arguments then
                    output = output .. ", "
                end
            end
            output = output .. ")\n"
        elseif statement.type == "if" then
            for i, case in ipairs(statement.cases) do
                if i == 1 then
                    output = output .. "if " .. unwrap_expression(case.condition) .. " then\n"
                else
                    output = output .. "elseif " .. unwrap_expression(case.condition) .. " then\n"
                end
                for _, stmt in ipairs(case.statements) do
                    output = output .. compile_statement(stmt)
                end
            end
            if #statement.elseblock > 0 then
                output = output .. "else\n"
                for _, stmt in ipairs(statement.elseblock) do
                    output = output .. compile_statement(stmt)
                end
            end
            output = output .. "end\n\n"
        end

        return output .. "\n"
    end

    local print_function = function(statement)
        local output

        local is_class = statement.class and true or false
        local is_constructor = is_class and statement.name == "constructor"

        if is_class then
            if is_constructor then
                output = "function " .. statement.class .. ":new("
            else
                output = "function " .. statement.class .. ":" .. statement.name .. "("
            end
        else
            output = "function " .. statement.name .. "("
        end

        for i, parameter in ipairs(statement.parameters) do
            output = output .. parameter.name
            if i < #statement.parameters then
                output = output .. ", "
            end
        end
        output = output .. ")\n"

        -- set self
        if is_constructor then
            output = output .. "local self = {}\n"
            output = output .. "setmetatable(self, { __index = " .. statement.class .. " })\n"
        end

        -- print statements
        for _, stmt in ipairs(statement.statements) do
            output = output .. compile_statement(stmt)
        end

        if is_constructor then
            output = output .. "return self\n"
        end

        output = output .. "end\n\n"

        return output
    end

    local print_class = function(statement)
        local output
        output = "local " .. statement.name .. " = {}\n"
        for _, field in ipairs(statement.fields or {}) do
            local value = field.value or "nil"
            output = output .. statement.name .. "." .. field.name .. " = " .. unwrap_expression(value) .. "\n"
        end
        output = output .. "\n"

        for _, func in ipairs(statement.functions or {}) do
            output = output .. print_function(func)
        end

        return output
    end

    local indent = 0
    for _, statement in ipairs(ast.statements) do

        if statement.type == "function" then
            output = output .. print_function(statement)
        elseif statement.type == "class" then
            output = output .. print_class(statement)
        elseif statement.type == "enum" then
            output = output .. "local " .. statement.name .. " = {\n"
            for i, item in ipairs(statement.items) do
                output = output .. "    " .. item .. " = " .. i
                if i < #statement.items then
                    output = output .. ",\n"
                end
            end
            output = output .. "\n}\n\n"
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

if tokens then
    local file = io.open("build/tokens.lua", "w")
    file:write(table_to_string(tokens))
    file:close()
end

-- parse tokens
local ast = parse(tokens)
--print_table(ast)

if ast then
    local file = io.open("build/ast.lua", "w")
    file:write(table_to_string(ast))
    file:close()
end

-- write file
if ast then
    local file = io.open("build/output.lua", "w")
    file:write(compile(ast))
    file:close()
end