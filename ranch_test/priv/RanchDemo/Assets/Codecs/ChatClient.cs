using UnityEngine;
using System.Collections;
using System;
using System.Text;

namespace yaski.codecs.chat {

	public partial class ChatClient {

		// To Server
		private void Connect()
		{
			Send (this, 3);
		}

		private void SendMessage(string message)
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
			byte s0;
			byte s1;
			byte s2;
		}
		partial void OnInit(Type0 arg);

		public struct Type1
		{
			long time;
			byte user;
			string message;
		}
		partial void OnReceiveMessage(Type1 arg);

		// Internal implementation

		public static void Call(ChatClient client, byte[] data, int length)
		{

		}

		public static void Send(ChatClient client, byte type)
		{

		}

		public static void Send(ChatClient client, byte type, byte[] data, int length)
		{
			
		}

	}

}
