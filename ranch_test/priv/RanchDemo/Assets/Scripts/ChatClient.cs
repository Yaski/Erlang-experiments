using UnityEngine;
using System.Collections;

namespace yaski.codecs.chat {

	public partial class ChatClient : MonoBehaviour {

		partial void OnInit(Type0 arg)
		{
			Debug.Log("On Init");
		}
		
		partial void OnReceiveMessage(Type1 arg)
		{
			Debug.Log("On Message From " + arg.user + " = " + arg.message);
		}

	}

}
