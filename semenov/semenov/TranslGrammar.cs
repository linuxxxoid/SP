using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MPTranslator
{
    class TranslRule : Prule
    {
        protected ArrayList RightP = null; //operation part

        public ArrayList rightP { get { return RightP; } set { RightP = value; } }
        public TranslRule(string Left, ArrayList Right, ArrayList RightP) : base(Left, Right)
        {
            this.RightP = RightP;
        }
    }
    class TranslGrammar : myGrammar
    {
        public ArrayList P = null;//operation symbols
        public TranslGrammar(ArrayList T, ArrayList V, string S0, ArrayList P) : base(T, V, S0)
        {
            this.P = P;
        }

        public void AddTrules(string Left, ArrayList Right, ArrayList P)
        {
            Prules.Add(new TranslRule(Left, Right, P));
        }

        public bool isPterm(string p)//проверка на операционный символ
        {
            foreach (string pi in this.P)
            {
                if (p == pi)
                    return true;
            }
            return false;
        }
    }
}
