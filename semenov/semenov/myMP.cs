using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;

namespace MPTranslator
{

    class DeltaQSigmaGamma
    {
        // структура Delta отображения
        public string LeftQ {get;set;} // = null;  // исходное состояние
        public string LeftT { get; set; }  // символ входной цепочки
        public string LeftZ { get; set; }  // верхний символ магазин
        public ArrayList RightQ { get; set; }  // множество следующих состояний
        public ArrayList RightZ { get; set; }  // множество символов магазина
               
        // Delta (  q1   ,   a    ,   z   ) = {  {q}   ,   {z1z2...} } 
        //         LeftQ    LeftT   LeftZ       RightQ       RightZ 
        public DeltaQSigmaGamma(string LeftQ, string LeftT, string LeftZ, ArrayList RightQ, ArrayList RightZ)
        {
            this.LeftQ = LeftQ;
            this.LeftT = LeftT;
            this.LeftZ = LeftZ;
            this.RightQ = RightQ;
            this.RightZ = RightZ;
        }

         /* Convert ArrayList to array.
          * 1.
        string[] array = list.ToArray(typeof(string)) as string[];

        Console.WriteLine(array.ToString());

            2.
        string mystr = string.Join(",", array);
        Console.WriteLine(mystr);
        */

        public void debug (){
          string rightQstr = string.Join("", this.RightQ.ToArray(typeof(string)) as string[]);
          string rightZstr = string.Join("", this.RightZ.ToArray(typeof(string)) as string[]);

          Console.WriteLine(" delta(Q {0},T {1},Z {2})          = (Q {3},Z {4})",
                              this.LeftQ, this.LeftT, this.LeftZ, rightQstr, rightZstr);      
        }

    } // end class Delta

    class DeltaQSigmaGammaSix : DeltaQSigmaGamma  {
        public DeltaQSigmaGammaSix(string LeftQ, string LeftT, string LeftZ, ArrayList RightQ, ArrayList RightZ, ArrayList RightSix) :
            base(LeftQ, LeftT, LeftZ, RightQ, RightZ)
        {
            this.rightSix = RightSix;
        }
        public ArrayList rightSix { get; set;}
    }


    class myMp : Automate  //МП = {}
    {
        // Q - множество состояний МП - автоматa
        // Sigma - алфавит входных символов
        // DeltaList - правила перехода 
        // Q0 - начальное состояние
        // F - множество конечных состояний
        public ArrayList Gamma = null;     //алфавит магазинных символов
  
        public Stack Z = null;

        public string ans1 = "";
        public string ans2 = "";

        //
        public myMp(ArrayList Q, ArrayList Sigma, ArrayList Gamma, string Q0, string z0, ArrayList F)
            : base(Q, Sigma, F, Q0)
        {
            this.Gamma = Gamma;

            this.Z = new Stack();
        //  Q0 = Q[0].ToString();  // начальное состояние
            Z.Push(z0);  // начальный символ в магазине
            this.F = F;       // пустое множество заключительных состояний
        }
        //
        public myMp(myGrammar KCgrammar)
            : base(new ArrayList() { "q" }, KCgrammar.T, new ArrayList() { }, "q")
        {
            this.Gamma = new ArrayList();
            this.Z = new Stack();
            foreach (string v1 in KCgrammar.V)   // магазинные символы
                Gamma.Add(v1);
            foreach (string t1 in KCgrammar.T)
                Gamma.Add(t1);
            Q0 = Q[0].ToString();  // начальное состояние
            Z.Push(KCgrammar.S0);  // начальный символ в магазине
            F = new ArrayList();  // пустое множество заключительных состояний

            DeltaQSigmaGamma delta = null;
            foreach (string v1 in KCgrammar.V)
            {            // сопоставление правил с отображениями
                ArrayList q1 = new ArrayList();
                ArrayList z1 = new ArrayList();
                foreach (Prule rule in KCgrammar.Prules)
                {
                    if (rule.leftNoTerm == v1)
                    {
                        Stack zb = new Stack();
                        ArrayList rr = new ArrayList(rule.rightChain);
                        rr.Reverse();
                        foreach (string s in rr)
                            zb.Push(s);
                        z1.Add(zb);
                        q1.Add(Q0);
                    }
                }
                delta = new DeltaQSigmaGamma(Q0, "e", v1, q1, z1);
                DeltaList.Add(delta);
            }
            foreach (string t1 in KCgrammar.T)
            {
                Stack e = new Stack();
                e.Push("e");
                delta = new DeltaQSigmaGamma(Q0, t1, t1, new ArrayList() { Q0 }, new ArrayList() { e });
                DeltaList.Add(delta);
            }
        }

        public virtual void addDeltaRule(string LeftQ, string LeftT, string LeftZ, ArrayList RightQ, ArrayList RightZ)
        {
            DeltaList.Add(new DeltaQSigmaGamma(LeftQ, LeftT, LeftZ, RightQ, RightZ));
        }

        public void addDeltaRule(string LeftQ, string LeftT, string LeftZ, ArrayList RightQ, ArrayList RightZ, ArrayList RightSix)
        {
            DeltaList.Add(new DeltaQSigmaGammaSix(LeftQ, LeftT, LeftZ, RightQ, RightZ, RightSix));
        }

        public virtual bool Execute(string str) {
            string currState = this.Q0;
            DeltaQSigmaGamma delta = null;
            int i = 0;
            str = str + "e"; // empty step

            //delta = findDelta(currState, "S");

            for (;;) {
//                
            
               // 
                //
               // delta.debug();
                //return false;
                //

                //if (delta == null) return false;
                //  
                //if ( Z.Peek().ToString()    ) {  // В ВЕРШИНЕ СТЕКА СИМВОЛ ? определить T   

                
                // }

                if (delta.LeftT != "e")  // И В ВЕРШИНЕ СТЕКА ТЕРМИНАЛЬНЫЙ СИМВОЛ
                {
                    for (; i < str.Length; ) {
                        this.Q = delta.RightQ;
                        currState = arrToStr(delta.RightQ);
                        //
                //        delta = findDelta(str[i].ToString());
                        //
                        if (delta.LeftZ == Z.Peek().ToString() && delta.RightZ[0].ToString() == "e")
                        {
                            this.Z.Pop();
                        }
                        else this.Z.Push(delta.LeftT);
                        i++;
                        break;
                    }
                }
                else if (delta.LeftT == "e")  // И В ВЕРШИНЕ СТЕКА НЕ ТЕРМИНАЛЬНЫЙ СИМВОЛ
                {
                    
                    this.Q = delta.RightQ;
                    this.Z.Pop();
                    if (this.Z.Count == 0) return true;
                    else return false;
                }
            } // end for
        } // end Execute

        //
        // поиск правила по состоянию. 

        public DeltaQSigmaGamma findDelta(string Q, string a) {
            foreach (DeltaQSigmaGamma delta in this.DeltaList) {
               if (delta.LeftQ == Q && delta.LeftT == a) return delta;
            }
            return null; // not find 
        }
        // поиск правила по символу в вершине  магазина
 
        public DeltaQSigmaGamma findDelta(string a) { // input string
            foreach (DeltaQSigmaGamma delta in this.DeltaList) {
                if (this.Z.Peek().ToString() == a && delta.LeftT == a) return delta;
            }
            return null; // not find 
        }


        //*** вспомогательные процедуры ***

        //объединение множеств A or B
        public ArrayList Unify(ArrayList A, ArrayList B)
        {
            ArrayList unify = A;
            foreach (string s in B)
                if (!A.Contains(s))
                    unify.Add(s);
            return unify;
        }

        //преобразование элементов массива в строку
        public string arrToStr(ArrayList array)
        {
            if (array.Equals(null)) return null;
            else
            {
                string newLine = "";
                foreach (string s in array)
                    newLine += s;
                return newLine;
            }
        }
        //проверка на принадлежность множествам автомата
        public bool isGamma(string v)
        {
            foreach (string vi in this.Gamma)
            {
                if (v == vi)
                    return true;
            }
            return false;
        }

        public bool isSigma(string t)
        {
            foreach (string ti in this.Sigma)
            {
                if (t == ti)
                    return true;
            }
            return false;
        }

        public string StackToString(Stack Z)
        {
            if (Z.Count == 0) return null;
            else
            {
                string newLine = "";
                Stack temp = new Stack();
                for (int i = 0; i < Z.Count; i++)
                {
                    temp.Push(Z.Pop());
                    newLine += Z.Peek();
                }
                for (int i = 0; i < temp.Count; i++)
                    Z.Push(temp.Pop());
                return newLine;
            }
        }



        // **   Debug   **
        public string DebugStack(Stack s)
        { // печать текущего состояния магазина
            string p = "|";
            Stack s1 = new Stack();
            while (s.Count != 0)
            {
                s1.Push(s.Pop());
                p = p + s1.Peek().ToString();
            }
            while (s1.Count != 0) s.Push(s1.Pop());
            return p;
        }

        public virtual void debugDelta()
        {
            Console.WriteLine("Deltarules :");
            if (this.DeltaList == null) { Console.WriteLine("null"); return; } 
           
            foreach (DeltaQSigmaGamma d in this.DeltaList) {
                    d.debug();
                    //Console.Write("( " + d.leftQ + " , " + d.leftT + " , " + d.leftZ + " )");
                    //Console.Write(" -> \n");
                    //Console.WriteLine("[ { " + arrToStr(d.rightQ) + " } , { " + arrToStr(d.rightZ) + " } ]");
            }
        }
    } // end class


    class translMp : myMp    //МП = {}
    {
        // Q - множество состояний МП - автоматa
        // Sigma - алфавит входных символов
        // DeltaList - правила перехода 
        // Q0 - начальное состояние
        // F - множество конечных состояний
        // ans - выходная строка
        public string ans = "";
        public string gamma0 = null;
        public translMp(ArrayList Q, ArrayList Sigma, ArrayList Gamma, string Q0, string Z0, ArrayList F)
            : base( Q, Sigma,  Gamma,  Q0, Z0, F)
        {
            this.Gamma = Gamma;
            this.Z = new Stack();
            gamma0 = Gamma[0].ToString();
            //Q0 = Q[0].ToString();  // начальное состояние
            Z.Push(gamma0);  // начальный символ в магазине
            this.F = F;       // пустое множество заключительных состояний
        }
        public translMp(myGrammar KCgrammar)
            : base(KCgrammar)
        {
            this.Gamma = new ArrayList();
            this.Z = new Stack();
            foreach (string v1 in KCgrammar.V)   // магазинные символы
                Gamma.Add(v1);
            foreach (string t1 in KCgrammar.T)
                Gamma.Add(t1);
            Q0 = Q[0].ToString();  // начальное состояние
            Z.Push(KCgrammar.S0);  // начальный символ в магазине
            F = new ArrayList();  // пустое множество заключительных состояний
            DeltaQSigmaGamma delta = null;
            foreach (string v1 in KCgrammar.V)
            {            // сопоставление правил с отображениями
                ArrayList q1 = new ArrayList();
                ArrayList z1 = new ArrayList();
                foreach (Prule rule in KCgrammar.Prules)
                {
                    if (rule.leftNoTerm == v1)
                    {
                        Stack zb = new Stack();
                        ArrayList rr = new ArrayList(rule.rightChain);
                        rr.Reverse();
                        foreach (string s in rr)
                        {
                            zb.Push(s);
                        }
                        z1.Add(zb);
                        q1.Add(Q0);
                    }
                }
                delta = new DeltaQSigmaGamma(Q0, "e", v1, q1, z1);
                DeltaList.Add(delta);
            }
            foreach (string t1 in KCgrammar.T)
            {
                Stack e = new Stack();
                e.Push("e");
                delta = new DeltaQSigmaGamma(Q0, t1, t1, new ArrayList() { Q0 }, new ArrayList() { e });
                DeltaList.Add(delta);
            }
        }
       public override bool Execute(string str) {
            string currState = this.Q0;
            DeltaQSigmaGamma delta = null;
            int i = 0;
            str = str + "e";
            for (; ; )  // empty step
            {
                delta = findDelta(currState, str[i].ToString());
                if (delta == null) return false;
                if (delta.LeftT != "e")
                {
                    for (; i < str.Length;)
                    {
                        this.Q = delta.RightQ;
                        currState = arrToStr(delta.RightQ);
                        if (delta.LeftZ == Z.Peek().ToString() && delta.RightZ[0].ToString() == "e")
                        {
                            ans1 += delta.LeftT;
                            this.Z.Pop();
                        }
                        else
                        {
                            ans2 += delta.LeftT;
                            this.Z.Push(delta.LeftT);
                        }

                        i++;
                        break;
                    }
                }
                else if (delta.LeftT == "e")
                {
                    this.Q = delta.RightQ;
                    this.Z.Pop();
                    if (this.Z.Count == 0)
                    {
                        for (int j = 0; j < ans2.Length; j++)
                        {
                            ans += ans2[j];
                            ans += ans1[j];
                        }
                        Console.WriteLine(ans);
                        return true;
                    }
                    else return false;
                }
            } // end for
        } // end Execute_  
    }//end class translMp
}