using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;

namespace MPTranslator
{

    abstract class Rule
    { // LeftNoTerm operator Right 
        public string LeftNoTerm = null;

        public Rule(string LeftNoTerm)
        {
            this.LeftNoTerm = LeftNoTerm;
        }

        public Rule() { }

        public string leftNoTerm
        {
            get { return LeftNoTerm; }
            set { LeftNoTerm = value; }
        }
    }// end Rule

    abstract class Automate
    {
        public ArrayList Q = null; // множество состояний
        public ArrayList Sigma = null; // алфавит
        public ArrayList DeltaList = null; //список правил перехода
        public string Q0 = null; //начальное состояние
        public ArrayList F = null; //конечное состояние

        public Automate() { }

        public Automate(ArrayList Q, ArrayList Sigma, ArrayList F, string q0)
        {
            this.Q = Q;
            this.Sigma = Sigma;
            this.Q0 = q0;
            this.F = F;
            this.DeltaList = new ArrayList();
        }

        public void AddRule(string state, string term, string nextState)
        {
            this.DeltaList.Add(new DeltaQSigma(state, term, nextState));
        }

        //для пустого символа "" currStates добавляются в ArrayList - ReachableStates

        private ArrayList EpsClosure(ArrayList currStates)
        {
            Debug("Eps-Closure", currStates);
            return EpsClosure(currStates, null);
        }

        // Все достижимые состояния из множества состояний states 
        // по правилам в которых ,LeftTerm = term
        private ArrayList EpsClosure(ArrayList currStates, ArrayList ReachableStates)
        {
            if (ReachableStates == null) ReachableStates = new ArrayList();
            ArrayList nextStates = null;
            ArrayList next = new ArrayList();
            int count = currStates.Count;
            //Console.WriteLine("count = " + count.ToString());
            for (int i = 0; i < count; i++)
            {
                nextStates = FromStateToStates(currStates[i].ToString(), "");
                //Debug("\nFrom", currStates[i].ToString());
                //Debug("NextStates", nextStates);
                // 1. если nextStates = null и это e-clouser  
                if (!ReachableStates.Contains(currStates[i].ToString()))
                {
                    ReachableStates.Add(currStates[i].ToString());
                    //Debug("Added currStates = ", currStates[i].ToString());
                }
                if (nextStates != null)
                {
                    //Debug("step R", currStates[i].ToString());
                    //Debug("Contains", ReachableStates.Contains(currStates[i].ToString()).ToString());
                    //1. из одного состояния возможен переход в несколько состояний,
                    //но это состояние в множестве должно быть только один раз,
                    //то есть для него выполняется операция объединения
                    foreach (string nxt in nextStates)
                    {
                        //Debug("nxt", nxt);
                        ReachableStates.Add(nxt);
                        next.Add(nxt);
                    }
                    //Debug("RS1", ReachableStates);
                }
            }
            //Debug("RS2", ReachableStates);
            if (nextStates == null) return ReachableStates;
            else return EpsClosure(next, ReachableStates);
        }
        //возвращает множество достижимых состояний по символу term 
        //из currStates за один шаг
        private ArrayList move(ArrayList currStates, string term)
        {
            ArrayList ReachableStates = new ArrayList();
            ArrayList nextStates = new ArrayList();
            foreach (string s in currStates)
            {
                nextStates = FromStateToStates(s, term);
                if (nextStates != null)
                    foreach (string st in nextStates)
                        if (!ReachableStates.Contains(st))
                            ReachableStates.Add(st);
            }
            return ReachableStates;
        }

        // Все состояния в которые есть переход из текущего состояния currState 
        // по символу term за один шаг
        private ArrayList FromStateToStates(string currState, string term)
        {
            ArrayList NextStates = new ArrayList();//{currState};
            bool flag = false;
            foreach (DeltaQSigma d in DeltaList)
            {
                //debugDeltaRule("AllRules", d);
                if (d.LeftNoTerm == currState && d.leftTerm == term)
                {
                    NextStates.Add(d.RightNoTerm);
                    //debugDeltaRule("FromStateToStates DeltaRules", d);
                    flag = true;
                }
            }
            if (flag) return NextStates;
            else return null;
        }

        private ArrayList config = new ArrayList();
        private ArrayList DeltaD = new ArrayList();//правила детерминированного автомата

        private ArrayList Dtran(ArrayList currState)
        {
            ArrayList statesSigma = null;
            ArrayList newState = null;

            for (int i = 0; i < Sigma.Count; i++)
            {
                statesSigma = move(currState, Sigma[i].ToString());

                Debug("move", statesSigma);

                newState = EpsClosure(statesSigma);
                Debug("Dtran " + i.ToString() + " " + Sigma[i].ToString(), newState);
                if (SetName(newState) != null)
                    DeltaD.Add(new DeltaQSigma(SetName(currState), Sigma[i].ToString(), SetName(newState)));
                debugDeltaRule("d", new DeltaQSigma(SetName(currState), Sigma[i].ToString(), SetName(newState)));
                if (config.Contains(SetName(newState)))
                    continue;
                config.Add(SetName(newState));
                Debug("config", config);

                Dtran(newState);
                Console.WriteLine("Building completed");

            }
            return null;
        }

        //построить Delta-правила ДКА
        public void BuildDeltaDKAutomate(myAutomate ndka)
        {
            this.Sigma = ndka.Sigma;
            this.DeltaList = ndka.DeltaList;
            ArrayList currState = EpsClosure(new ArrayList() { ndka.Q0 });

            //Debug("step 1", currState);

            config.Add(SetName(currState));
            //Debug("name",SetName(currState));
            Dtran(currState);
            this.Q = config;
            this.Q0 = this.Q[0].ToString();
            this.DeltaList = DeltaD;
            this.F = getF(config, ndka.F);

            /*  this.Q = makeNames(config);
              this.Q0 = this.Q[0].ToString();
              this.DeltaList = NameRules(DeltaD);
              this.F = makeNames(getF(config, ndka.F));*/

        }

        private ArrayList getF(ArrayList config, ArrayList F)
        {
            ArrayList newF = new ArrayList();
            foreach (string f in F)
            {
                foreach (string name in this.config)
                {
                    if (name != null && name.Contains(f))
                    {
                        //Debug("substr",name);
                        //Debug("f", f);
                        newF.Add(name);
                    }
                }
            }
            return newF;
        }

        //состояние StateTo достижимо по дельта-правилам из состояния currState
        private bool ReachableStates(string currState, string StateTo)
        {
            string nextstate = currState;
            bool b = true;
            if (currState == StateTo) return false;
            while (b)
            {
                b = false;
                foreach (DeltaQSigma d in this.DeltaList)
                {
                    if (nextstate == d.leftNoTerm)
                    {
                        if (nextstate == StateTo) return true;
                        nextstate = d.RightNoTerm;
                        b = true;
                        break;
                    }
                }
            }
            return false;
        }//end ReachableStates

        private Hashtable names = new Hashtable();

        private ArrayList makeNames(ArrayList config)
        {
            ArrayList NewNames = new ArrayList();
            for (int i = 0; i < config.Count; i++)
            {
                NewNames.Add(i.ToString());
            }
            return NewNames;
        }

        private ArrayList NameRules(ArrayList DeltaRules)
        {
            ArrayList newRules = new ArrayList();
            string newLeftNoTerm = null;

            string newRight = null;

            foreach (DeltaQSigma d in DeltaRules)
            {
                for (int i = 0; i < this.config.Count; i++)
                {
                    if (d.LeftNoTerm == this.config[i].ToString())
                        newLeftNoTerm = this.Q[i].ToString();
                }
                for (int i = 0; i < this.Q.Count; i++)
                {
                    if (d.rightNoTerm == this.config[i].ToString().ToString())
                        newRight = this.Q[i].ToString();
                }
                newRules.Add(new DeltaQSigma(newLeftNoTerm, d.LeftTerm, newRight));
            }
            return newRules;
        }

        private string SetName(ArrayList list)
        {
            string line = null;
            if (list == null) { return ""; }
            for (int i = 0; i < list.Count; i++)
                line += list[i].ToString();
            return line;
            /*  Debug("key", line);
              if (names.ContainsKey(line)){
                object value = names[line];
                Console.WriteLine("value : " + names[line].ToString()); 
                return value.ToString(); 
              }              
              else {
                  names.Add(line, N++);
                  return N.ToString();
              }*/
        }


        ///***  Debug ***///        
        public void Debug(string step, string line)
        {
            Console.Write(step + ": ");
            Console.WriteLine(line);
        }

        public void Debug(string step, ArrayList list)
        {
            Console.Write(step + ": ");
            if (list == null) { Console.WriteLine("null"); return; }
            for (int i = 0; i < list.Count; i++)
                if (list[i] != null)
                    Console.Write(list[i].ToString() + " ");
            Console.Write("\n");
        }

        public void Debug(ArrayList list)
        {
            Console.Write("{ ");
            if (list == null) { Console.WriteLine("null"); return; }
            for (int i = 0; i < list.Count; i++)
                Console.Write(list[i].ToString() + " ");
            Console.Write(" }\n");
        }

        public void debugDeltaRule(string step, DeltaQSigma d)
        {
            Console.WriteLine(step + ": (" + d.leftNoTerm + " , " + d.leftTerm + " ) -> " + d.RightNoTerm);
        }

        public void DebugAuto()
        {
            Console.WriteLine("\nAutomate config:");
            Debug("Q", this.Q);
            Debug("Sigma", this.Sigma);
            Debug("Q0", this.Q0);
            Debug("F", this.F);
            Console.WriteLine("DeltaList:");
            foreach (DeltaQSigma d in this.DeltaList)
                debugDeltaRule("", d);
        }

    } //end Automate

    abstract class Grammar
    {

        public string S0 = null;    //начальный символ
        public ArrayList T = null;         //список терминалов
        public ArrayList V = null;         //список нетерминалов
        public ArrayList Prules = null;    //список правил порождения

        public Grammar() { }

        public Grammar(ArrayList T, ArrayList V, string S0)
        {
            this.T = T;
            this.V = V;
            this.S0 = S0;
        }

        abstract public string Execute(); // abstract

        public void AddRule(string LeftNoTerm, ArrayList right)
        {
            this.Prules.Add(new Prule(LeftNoTerm, right));
        }

        //Lina Veltman course work
        public myAutomate Transform()
        { 
            //Конечное множество состояний КА = множество нетерминалов в объединении с заключительным состоянием qf
            ArrayList Q = this.V;
            Q.Add("qf");
            //Начальное состояние КА совпадает с начальным символом АГ
            string q0 = this.S0;
            //Пустое множество заключительных состояний
            ArrayList F = new ArrayList();
            //Формируем множество заключительных состояний
            //Проходимся по всем правилам
            foreach (Prule p in this.Prules)
            {
                //Если нашлось правило вида: S0 -> ε
                if (p.LeftNoTerm.Contains("S0") && p.RightChain.Contains("e"))
                {
                    //Добавляем в множество закл.сост. F: S0 и qf
                    F = new ArrayList { p.LeftNoTerm, "qf" };
                    break;
                }
                //Иначе добавляем в множество F только qf
                else if (p.LeftNoTerm.Contains("S0"))
                {
                    F = new ArrayList { "qf" };
                    break;
                }
            }
            //Строим конечный автомат по образованным множествам
            myAutomate KA = new myAutomate(Q, this.T, F, q0);
            bool flag = true;
            //Строим функции переходов δ
            foreach (Prule p in this.Prules)
            {
                //Если существует правило порождения вида S0 -> ε
                if (flag && p.LeftNoTerm.Contains("S0") && p.RightChain.Contains("e"))
                {
                    //Строим функцию перехода δ(S0, e) = qf
                    KA.AddRule(p.LeftNoTerm, "e", "qf");
                    flag = false;
                }
                //Входные алфавиты КА и АГ совпадают
                //Проходимся по входному алфавиту автоматной грамматики
                foreach (string t in this.T)
                {
                    //Если справа есть символ и это терминал
                    if (p.RightChain.Contains(t) && NoTermReturn(p.RightChain) == null)
                        //Строим функцию перехода δ(НетерминалСлева, терминал) = qf
                        KA.AddRule(p.LeftNoTerm, t, "qf");
                    //Если справа есть терминал и нетерминал
                   else if (p.RightChain.Contains(t) && NoTermReturn(p.RightChain) != null)
                        //Строим функцию перехода δ(НетерминалСлева, терминал) = НетерминалСправа
                        KA.AddRule(p.LeftNoTerm, t, NoTerminal(p.RightChain));
                }
            }
            //Возвращаем построенный Конечный автомат
            return KA;
        }

        //определение множествa производящих нетерминальных
        //символов
        private ArrayList producingSymb()
        {
            ArrayList Vp = new ArrayList();
            foreach (Prule p in this.Prules)
            {
                bool flag = true;
                foreach (string t in this.T)
                    if (p.RightChain.Contains(t))
                        flag = false;
                if (!flag && !Vp.Contains(p.leftNoTerm)) Vp.Add(p.leftNoTerm);
            }
            return Vp;
        }

        //определение множества достижимых символов за 1 шаг
        private ArrayList ReachableByOneStep(string state)
        {
            ArrayList Reachable = new ArrayList() { state };
            ArrayList tmp = new ArrayList();
            int flag = 0;
            foreach (Prule p in this.Prules)
            {
                if (p.LeftNoTerm == state)
                    for (int i = 0; i < p.rightChain.Count; i++)
                        for (int j = 0; j < Reachable.Count; j++)
                            if (p.rightChain[i].ToString() != Reachable[j].ToString())
                            {
                                tmp.Add(p.rightChain[i].ToString());// Debug(tmp);Console.WriteLine("");
                                break;
                            }
            }
            foreach (string s in tmp)
            {
                flag = 0;
                for (int i = 0; i < Reachable.Count; i++)
                    if (Reachable[i].ToString() == s)
                        flag = 1;
                if (flag == 0) Reachable.Add(s);
            }
            return Reachable;
        }

        //определение множества достижимых символов
        private ArrayList Reachable(string StartState)
        {
            ArrayList Vr = new ArrayList() { this.S0 };
            ArrayList nextStates = ReachableByOneStep(StartState);
            Debug("NEXT", nextStates);
            ArrayList NoTermByStep = NoTermReturn(nextStates);
            Debug("NoTermByStep", NoTermByStep);
            Vr = Unify(Vr, NoTermByStep);
            foreach (string NoTerm in NoTermByStep)
            {
                Vr = Unify(Vr, ReachableByOneStep(NoTerm));
            }
            return Vr;
        }

        //удаление бесполезных символов
        public myGrammar unUsefulDelete()
        {
            Console.WriteLine("\t\tDeleting unuseful symbols");
            Console.WriteLine("Executing: ");
            //построить множество производящих символов
            ArrayList Vp = producingSymb(); Debug("Vp", Vp);
            ArrayList P1 = new ArrayList();
            ArrayList TorVp = Unify(Vp, this.T); Debug("TorVp", TorVp);
            Console.WriteLine("\nP1 building:\n");
            foreach (Prule p in this.Prules)
            {
                DebugPrule(p);
                ArrayList SymbInRule = SymbInRules(p);//Debug("SinR", SymbInRule); 
                bool flag = true;
                foreach (string s in SymbInRule)
                    if (!TorVp.Contains(s) || TermReturn(p.RightChain) == null)
                        flag = false;
                if (flag)
                {
                    P1.Add(p); Console.WriteLine("Added");
                }
            }
            //построить множество достижимых символов
            ArrayList Vr = Reachable(this.S0); Debug("Vr", Vr);
            ArrayList T1 = intersection(this.T, Vr); Debug("T1", Vr);
            ArrayList V1 = intersection(Vr, this.V); Debug("V1", Vr);
            ArrayList P2 = new ArrayList();
            Console.WriteLine("\nP2 building:\n");
            foreach (Prule p in P1)
            {
                DebugPrule(p);
                ArrayList SymbInRule = SymbInRules(p);
                bool flag = true;
                foreach (string s in SymbInRule)
                    if (!Vr.Contains(s))
                        flag = false;
                if (flag)
                {
                    P2.Add(p); Console.WriteLine("Added");
                }
            }
            Console.WriteLine("Unuseful symbols have been deleted");
            return new myGrammar(T1, V1, P2, this.S0);
        }

        //построение множества укорачивающих нетерминалов
        private ArrayList ShortNoTerm()
        {
            ArrayList Ve = new ArrayList();
            foreach (Prule p in this.Prules)
            {
                if (p.rightChain.Contains(""))
                    Ve.Add(p.leftNoTerm);
            }
            int i = 0;///!!!
           // if (Ve.Count == 0) return null;

/*                                 
          foreach (string v in Ve) { 
           //  if ( FromWhat(vg()) != null) {
                Ve = Unify(Ve, FromWhat(v.ToString()));
           //  } 
           }          
 */           
            if (Ve.Count != 0)
                //Console.WriteLine("  {0}",Ve.Count);                
                while ( (FromWhat(Ve[i].ToString()) != null) && (Ve.Count < i ) ) 
                {
                    Ve = Unify(Ve, FromWhat(Ve[0].ToString()));
                    i++;                    
                }
            Debug("Ve",Ve);
  
            return Ve;
        }

        //удаление эпсилон правил
        public myGrammar EpsDelete()
        {
            Console.WriteLine("\t\tDeleting epsylon rules");
            Console.WriteLine("Executing: ");
            ArrayList Ve = ShortNoTerm(); Debug("Ve", Ve);
            ArrayList P1 = new ArrayList();
            ArrayList V1 = this.V;
            foreach (Prule p in Prules)
            {
                if (!ContainEps(p))
                {
                    //DebugPrule(p);
                    P1.Add(p);
                    Prule p1 = new Prule(p.leftNoTerm, TermReturn(p.rightChain));
//                    DebugPrule(p1);
                    if (p.rightChain.Count != 1)
                    {
                        Console.WriteLine("No contain");
                        P1.Add(p1);
                    }
                    else Console.WriteLine("Contain");
                }
            }
            if (Ve.Contains(this.S0))
            {
                V1.Add("S1");
                P1.Add(new Prule("S1", new ArrayList() { this.S0 }));
                P1.Add(new Prule("S1", new ArrayList() { "" }));
                return new myGrammar(this.T, V1, P1, "S1");
            }
            else
                return new myGrammar(this.T, V1, P1, this.S0);
        }

        //удаление цепных правил
        public myGrammar ChainRuleDelete()
        {
            Console.WriteLine("\t\tDeleting chain rules");
            Console.WriteLine("Executing: ");
            ArrayList NoChainRules = new ArrayList();
            ArrayList ChainRules = new ArrayList();
            foreach (Prule p in this.Prules)
            {
                if (TermReturn(p.rightChain) != null)
                    NoChainRules.Add(p);
                else
                    ChainRules.Add(p);
            }

            Console.Write("Chine Rules: ");
            if (ChainRules.Count == 0) Console.WriteLine("null");
            else
            {
                Console.WriteLine("");
                foreach (Prule p in ChainRules) DebugPrule(p);

                foreach (Prule chrule in ChainRules)
                    foreach (Prule p in NoChainRules)
                    {
                        if (p.RightChain.Contains(chrule.leftNoTerm))
                            for (int i = 0; i < p.RightChain.Count; i++)
                            {
                                if (p.RightChain[i].ToString() == chrule.leftNoTerm)
                                    p.RightChain[i] = chrule.RightChain[0].ToString();
                            }
                    }
            }
            ArrayList P = new ArrayList();
            foreach (Prule p in NoChainRules)
                if (!P.Contains(p)) P.Add(p);
            return new myGrammar(this.T, this.V, P, this.S0);
        }

        //удаление левой рекурсии
        public myGrammar LeftRecursDelete()
        {
            Console.WriteLine("\t\tDeleting left Recurs");
            Console.WriteLine("Executing: ");
            ArrayList LeftRecurs = new ArrayList();
            ArrayList P1 = new ArrayList();
            int i = 0;
            foreach (Prule p in this.Prules)
            {
                if (p.rightChain.Contains(p.leftNoTerm) && p.RightChain[0].ToString() == p.leftNoTerm)
                {
                    DebugPrule(p);
                    LeftRecurs.Add(p);
                }
                else P1.Add(p);
            }
            foreach (Prule p in LeftRecurs)
            {
                ArrayList right = new ArrayList();
                string alfa = "";
                for (int j = 1; j < p.RightChain.Count; j++)
                {
                    alfa += p.RightChain[j].ToString();
                }
                Debug("alfa", alfa);
                if (alfa == null) { }
                else
                {
                    P1.Add(new Prule(p.leftNoTerm, new ArrayList() { "S" + i.ToString() }));
                    this.V.Add("S" + i.ToString());
                    for (int k = 0; k < alfa.Length; k++)
                    {
                        right.Add(alfa.Substring(k, 1));
                    }
                    right.Add("S" + i.ToString());
                    P1.Add(new Prule("S" + i.ToString(), right));
                }
                i++;
            }
            return new myGrammar(this.T, this.V, P1, this.S0);
        }



        // **   Debug   **
        public void DebugPrules()
        {
            Console.WriteLine("Prules:");
            foreach (Prule p in this.Prules)
            {
                string right = "";
                for (int i = 0; i < p.rightChain.Count; i++)
                    right += p.rightChain[i].ToString();
                Console.WriteLine(p.leftNoTerm + " -> " + right);
            }
        }

        public void DebugPrule(Prule p)
        {
            string right = "";
            for (int i = 0; i < p.rightChain.Count; i++)
                right += p.rightChain[i].ToString();
            Console.WriteLine(p.leftNoTerm + " -> " + right + " ");
        }

        public void Debug(string step, ArrayList list)
        {
            Console.Write(step + " : ");
            if (list == null) Console.WriteLine("null");
            else
                for (int i = 0; i < list.Count; i++)
                    Console.Write(list[i].ToString() + " ");
            Console.WriteLine("");
        }

        public void Debug(string step, string line)
        {
            Console.Write(step + " : ");
            Console.WriteLine(line);
        }

        //откуда можем прийти в состояние
        private ArrayList FromWhat(string state)
        {
            ArrayList from = new ArrayList();
            bool flag = true;
            foreach (Prule p in this.Prules)
            {
                if (p.RightChain.Contains(state))
                {
                    from.Add(p.leftNoTerm);
                    flag = false;
                }
            }
            if (flag) return null;
            else return from;
        }

        //объединение множеств A or B
        private ArrayList Unify(ArrayList A, ArrayList B)
        {
            ArrayList unify = A;
            foreach (string s in B)
                if (!A.Contains(s))
                    unify.Add(s);
            return unify;
        }

        //пересечение множеств A & B
        private ArrayList intersection(ArrayList A, ArrayList B)
        {
            ArrayList intersection = new ArrayList();
            foreach (string s in A)
                if (B.Contains(s))
                    intersection.Add(s);
            return intersection;
        }

        //Нетерминальные символы из массива
        //
        // !!!!Б Ы Л О!!!
        //
        //private ArrayList NoTermReturn(ArrayList array)
        //{
        //    ArrayList NoTerm = new ArrayList();
        //    foreach (string s in array)
        //        if (this.V.Contains(s))
        //            NoTerm.Add(s);
        //    return NoTerm;
        //}
        //
        // !!!С Т А Л О!!!
        private ArrayList NoTermReturn(ArrayList array)
        {
            ArrayList NoTerm = new ArrayList();
            bool flag = true;//added
            foreach (string s in array)
               if (this.V.Contains(s))
                {
                    flag = false;//added
                    NoTerm.Add(s);
                }
            if (flag) return null;//added
            else return NoTerm;
        }
        //added for KA by Lina Veltman
        private string NoTerminal(ArrayList array)
        {
            string NoTermin = "";
            foreach (string s in array)
            {
                if (this.V.Contains(s))
                    NoTermin = s;
            }
            return NoTermin;
        }


        //терминальные символы из массива
        private ArrayList TermReturn(ArrayList A)
        {
            ArrayList Term = new ArrayList();
            bool flag = true;
            foreach (string t in this.T)
                if (A.Contains(t))
                {
                    flag = false;
                    Term.Add(t);
                }
            if (flag) return null;
            else return Term;
        }

        //все символы в правиле
        private ArrayList SymbInRules(Prule p)
        {
            ArrayList SymbInRules = new ArrayList() { p.LeftNoTerm };
            for (int i = 0; i < p.rightChain.Count; i++)
                SymbInRules.Add(p.rightChain[i].ToString());
            return SymbInRules;
        }

        //проверка пустоты правой цепочки
        private bool ContainEps(Prule p)
        {
            if (p.rightChain.Contains("")) return true;
            return false;
        }

    }//end abstract class Grammar
}