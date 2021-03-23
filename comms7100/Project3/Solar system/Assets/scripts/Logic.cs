using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Logic : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        //This needs to find out what scripts you are using and then call them.
        //may usee my bridge?
        //tbd
        
    }

    // Update is called once per frame
    void Update()
    {
        //This will grab the current locations

        //then calculate the changes based on that
    }

    void LateUpdate() { 
        //Late update is always called after update.
        //it will be use to actually update the locations of the planets
    
    }
}
