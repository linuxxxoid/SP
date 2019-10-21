using System;
using System.Collections.Generic;
using System.Linq;
using System.Collections;
using System.Text;

namespace MPTranslator
{

    class DeltaQSigma : Rule
    { //Q x Sigma
        public string LeftTerm = null;
        public string RightNoTerm = null;

        public DeltaQSigma(string LeftNoTerm, string LeftTerm, string RightNoTerm) :
            base(LeftNoTerm)
        {
            this.LeftTerm = LeftTerm;
            this.RightNoTerm = RightNoTerm;

        }

        public string leftTerm
        {
            get { return LeftTerm; }
            set { LeftTerm = value; }
        }

        public string rightNoTerm
        {
            get { return RightNoTerm; }
            set { RightNoTerm = value; }
        }

    }//end DeltaQSigma

    class myAutomate : Automate
    {
        public myAutomate(ArrayList Q, ArrayList Sigma, ArrayList F, string q0) :
            base(Q, Sigma, F, q0)
        { }

        public myAutomate() : base() { }


        public void Execute(string chineSymbol)
        {
            string currState = this.Q0;
            int flag = 0;
            int i = 0;
            for (; i < chineSymbol.Length; i++)
            {
                flag = 0;
                foreach (DeltaQSigma d in this.DeltaList)
                {
                    if (d.leftNoTerm == currState && d.leftTerm == chineSymbol.Substring(i, 1))
                    {
                        currState = d.RightNoTerm;
                        flag = 1;
                        break;
                    }
                }
                if (flag == 0) break;
            } // end for
            Console.WriteLine("Length: " + chineSymbol.Length);
            Console.WriteLine(" i :" + i.ToString());
            Debug("curr", currState);
            if (this.F.Contains(currState) && i == chineSymbol.Length)
                Console.WriteLine("chineSymbol belongs to language");
            else
                Console.WriteLine("chineSymbol doesn't belong to language");
        } // end Execute     
    } // KAutomate
}