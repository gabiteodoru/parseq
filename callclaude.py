import os
import subprocess
import tempfile
import uuid

def ask_claude(question: str) -> str:
    """
    Ask Claude a question by creating a temp directory and running claude -p
    Returns Claude's response as a string.
    """
    # Create a unique subdirectory name
    subdir_name = f"claude_session_{uuid.uuid4().hex[:8]}"
    subdir_path = os.path.join(os.getcwd(), subdir_name)
    
    # Create the subdirectory
    os.makedirs(subdir_path, exist_ok=True)
    
    # Change to the subdirectory
    original_cwd = os.getcwd()
    os.chdir(subdir_path)
    
    try:
        # Run claude -p with the question (properly quoted)
        # Use setsid and clean environment to avoid deadlocks when called from MCP server
        clean_env = {
            'PATH': os.environ.get('PATH'),
            # 'ANTHROPIC_API_KEY': os.environ.get('ANTHROPIC_API_KEY'),
            'HOME': os.environ.get('HOME', '/tmp'),
        }
        
        result = subprocess.run(
            ['setsid'] + '''claude --disallowedTools Agent,Bash,Edit,Glob,Grep,LS,MultiEdit,NotebookEdit,NotebookRead,Read,Task,TodoRead,TodoWrite,WebFetch,WebSearch,Write --strict-mcp-config --mcp-config {"mcpServers":{}} -p'''.split()+[question],
            #['claude', '-p', question],
            capture_output=True,
            text=True,
            timeout=30,  # 30 second timeout
            env=clean_env,
            stdin=subprocess.DEVNULL
        )
        
        if result.returncode == 0:
            return result.stdout.strip()
        else:
            return f"Error: {result.stderr.strip()}"
            
    except subprocess.TimeoutExpired:
        return "Error: Claude request timed out"
    except FileNotFoundError:
        return "Error: 'claude' command not found"
    except Exception as e:
        return f"Error: {str(e)}"
    finally:
        # Change back to original directory
        os.chdir(original_cwd)