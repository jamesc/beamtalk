#!/usr/bin/env python3
"""
Demo script showing ProtoObject in action via the REPL.
This simulates what a user would see in an interactive session.
"""

import socket
import json
import time

def send_request(sock, request_type, **kwargs):
    """Send a JSON request to the REPL and get response."""
    request = {"type": request_type, **kwargs}
    sock.sendall(json.dumps(request).encode() + b'\n')
    response = sock.recv(8192).decode().strip()
    return json.loads(response)

def main():
    # Connect to REPL backend
    print("🔌 Connecting to REPL backend on port 9000...")
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 9000))
    print("✅ Connected!\n")
    
    # Load the Counter class
    print("📦 Loading Counter class...")
    resp = send_request(sock, "load", path="tests/e2e/fixtures/counter.bt")
    if resp['type'] == 'loaded':
        print(f"✅ Loaded Counter class")
    else:
        print(f"❌ Failed: {resp.get('message', 'Unknown error')}")
        return
    
    # Load the SimpleProxy class
    print("📦 Loading SimpleProxy class (with doesNotUnderstand)...")
    resp = send_request(sock, "load", path="tests/e2e/fixtures/simple_proxy.bt")
    if resp['type'] == 'loaded':
        print(f"✅ Loaded SimpleProxy class\n")
    else:
        print(f"❌ Failed: {resp.get('message', 'Unknown error')}")
        return
    
    # Demonstrate ProtoObject features
    print("=" * 60)
    print("DEMONSTRATION: ProtoObject Features")
    print("=" * 60)
    
    # 1. Show 'class' message working on primitives
    print("\n1️⃣  Testing 'class' message on primitives:")
    print("    > 42 class")
    resp = send_request(sock, "eval", expression="42 class")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > 'hello' class")
    resp = send_request(sock, "eval", expression="'hello' class")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > true class")
    resp = send_request(sock, "eval", expression="true class")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    # 2. Spawn and test Counter
    print("\n2️⃣  Spawning Counter actor:")
    print("    > counter := Counter spawn")
    resp = send_request(sock, "eval", expression="counter := Counter spawn")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > counter class")
    resp = send_request(sock, "eval", expression="counter class")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    # 3. Spawn and test SimpleProxy
    print("\n3️⃣  Spawning SimpleProxy actor:")
    print("    > proxy := SimpleProxy spawn")
    resp = send_request(sock, "eval", expression="proxy := SimpleProxy spawn")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > proxy class")
    resp = send_request(sock, "eval", expression="proxy class")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    # 4. Test identity comparisons
    print("\n4️⃣  Testing identity (== and ~=):")
    print("    > 42 == 42")
    resp = send_request(sock, "eval", expression="42 == 42")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > 42 ~= 43")
    resp = send_request(sock, "eval", expression="42 ~= 43")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    # 5. Call a method directly
    print("\n5️⃣  Calling Counter methods directly:")
    print("    > counter increment")
    resp = send_request(sock, "eval", expression="counter increment")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > counter getValue")
    resp = send_request(sock, "eval", expression="counter getValue")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    # 6. Demonstrate doesNotUnderstand via proxy
    print("\n6️⃣  Testing doesNotUnderstand forwarding:")
    print("    > proxy setTarget: counter")
    resp = send_request(sock, "eval", expression="proxy setTarget: counter")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
    
    print("    > proxy increment  (forwarded to counter via doesNotUnderstand)")
    resp = send_request(sock, "eval", expression="proxy increment")
    if resp['type'] == 'result':
        print(f"    => {resp['value']}")
        print("    ℹ️  This message was forwarded using perform:withArguments:")
    
    print("\n" + "=" * 60)
    print("✅ ProtoObject demonstration complete!")
    print("=" * 60)
    
    sock.close()

if __name__ == '__main__':
    try:
        main()
    except ConnectionRefusedError:
        print("❌ Could not connect to REPL backend.")
        print("   Make sure REPL is running: cargo run -- repl")
    except Exception as e:
        print(f"❌ Error: {e}")
