using UnityEngine;
using System.Collections;
using System;
using System.Text;

namespace yaski.codecs.chat {

	public partial class ChatClient {

		// -===== To Server =====-

		private void Connect()
		{
			Send (this, 3);
		}

		private void Say(string message)
		{
			var data = Encoding.UTF8.GetBytes(message);
			Send (this, 6, data, data.Length);
		}

		private void Close()
		{
			Send (this, 2);
		}

		// -===== From Server =====-

		public struct Type0
		{
			public byte s0;
			public byte s1;
			public byte s2;
			public long time;
		}
		partial void OnInit(Type0 arg);

		public struct Type1
		{
			public long time;
			public byte user;
			public string message;
		}
		partial void OnReceiveMessage(Type1 arg);

		// Internal implementation

		public static void Call(ChatClient client, byte[] data, int length)
		{
			if (length > 0) {
				switch (data[0])
				{
				case 0:
					byte s0 = data[1];
					byte s1 = data[2];
					byte s2 = data[3];
					client.OnInit(new Type0{ s0 = s0, s1 = s1, s2 = s2 });
					break;
				case 2:
					long time = data[1];
					byte user = data[2];
					string message = data[3].ToString();
					client.OnReceiveMessage(new Type1{ time = time, user = user, message = message });
					break;
				}
			}
		}

		public static void Send(ChatClient client, byte type)
		{

		}

		public static void Send(ChatClient client, byte type, byte[] data, int length)
		{
			
		}

	}
}
