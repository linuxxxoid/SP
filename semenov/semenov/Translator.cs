using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MPTranslator
{
    class DeltaOsimbol : DeltaQSigmaGamma // дельта правило для трансляции
    {

        private ArrayList RightO = null;// множество символов перевода

        public ArrayList rightO { get { return RightO; } set { RightO = value; } }

        // Delta (  q1   ,   a    ,   z   ) = {  {q}   ,   {z1z2...} }
        // Delta (  q1   ,   a    ,   z   ) = {  {q}   ,   {z1z2...}, {b1b2.....} }  // RightO b1,b2 выходные операционные символы

        //         LeftQ    LeftT   LeftZ       RightQ       RightZ   RightNew
        public DeltaOsimbol(string LeftQ, string LeftT, string LeftZ, ArrayList RightQ, ArrayList RightZ, ArrayList RightO)
            : base(LeftQ, LeftT, LeftZ, RightQ, RightZ)
        {
            this.RightO = RightO;
        }
    }

    class Translator : myMp
    {
        public ArrayList Petta = new ArrayList(); //массив операционного алфавита 
        public Translator(TranslGrammar table)
            : base(new ArrayList() { "q" }, table.T, new ArrayList() { }, "q", "z0",   new ArrayList() { })
        {
            //инициализация магазинных и операционных символов
            foreach (string vi in table.V)
                Gamma.Add(vi);
            foreach(string ti in table.T)
                Gamma.Add(ti);
            foreach (string pi in table.P)
            {
                Gamma.Add(pi);
                Petta.Add(pi);
            }
            Z.Push(table.S0);
            DeltaOsimbol delta = null;
            foreach(string vi in table.V)//инициализация дельта преобразований
            {
                ArrayList z_arr = new ArrayList();
                foreach(TranslRule t in table.Prules)
                {
                    if (t.leftNoTerm != vi)
                        continue;
                    ArrayList rz = new ArrayList();
                    string nonminal = null;
                    foreach(string term in t.rightChain)
                    {
                        if (table.isTerm(term))
                            rz.Add(term); 
                        else if (term == "e")
                        {
                            rz.Add(term);
                            break;
                        }
                        else
                        {
                            nonminal = term;
                            foreach (string pterm in t.rightP)
                            {
                                if (table.isPterm(pterm))
                                    rz.Add(pterm);
                                else
                                    rz.Add(nonminal);
                            }
                        }
                    }
                    z_arr.Add(rz);
                }
                foreach (ArrayList rightZ in z_arr)
                {
                    delta = new DeltaOsimbol(Q0, "e", vi, Q, rightZ, new ArrayList() { "e" });
                    DeltaList.Add(delta);
                }
            }

            foreach (string ti in table.T)//deltaosimbol for terminals
            {
                delta = new DeltaOsimbol(Q0, ti, ti, new ArrayList() { Q0 }, new ArrayList() { "e" }, new ArrayList() { "e" });
                DeltaList.Add(delta);
            }

            int i = 0;
            foreach (string pi in table.P)//deltaosimbol for operation symbols
            {
                delta = new DeltaOsimbol(Q0, "e", pi, new ArrayList() { Q0 }, new ArrayList() { "e" }, new ArrayList() { table.T[i] });
                DeltaList.Add(delta);
                i++;
            }
        }

        public string Translation(string str)//функция перевода входной строки
        {
            ArrayList translated = new ArrayList();
            DeltaOsimbol ex_delta = null;
            str = str + "e";
            for (int i = 0; i < str.Length; )
            {
                if (str[i].ToString() == "e" && Z.Count == 1)//остался только q в магазине(см конструкторы)
                    return arrToStr(translated);
                if (!isSigma(str[i].ToString()))
                    return "строка не принадлежит языку";
                ex_delta = find_exDelta(Z.Peek().ToString());
                if (ex_delta == null)
                    return "строка не принадлежит языку";
                if (isSigma(ex_delta.LeftZ))//in LeftZ - sigma symbol
                {
                    if (ex_delta.LeftZ == str[i].ToString())
                    {
                        Z.Pop();
                        i++;
                        continue;
                    }
                    else
                        return "строка не принадлежит языку";
                }

                else if (isPetta(ex_delta.LeftZ))//in LeftZ - petta symbol
                {
                    Z.Pop();
                    translated.Add(arrToStr(ex_delta.rightO));
                }

                else
                {
                    //предположим разворачиваем по правилу схемы, если верх стэка совпадет с термом в строке, то развернули правильно
                    //если не совпадет - предполагаем следующее правило схемы
                    ArrayList res_right = new ArrayList();
                    if (ex_delta.RightZ[0].ToString() != str[i].ToString())
                    {
                        foreach (DeltaOsimbol tmp in this.DeltaList)
                        {
                            if (tmp.LeftZ == ex_delta.LeftZ)
                            {

                                if (tmp.RightZ[0].ToString() == "e")
                                {
                                    foreach (string z in tmp.RightZ)
                                        res_right.Add(z);
                                }
                                else if (tmp.RightZ[0].ToString() == str[i].ToString())
                                {
                                    foreach (string z in tmp.RightZ)
                                        res_right.Add(z);
                                    break;
                                }
                            }
                        }
                    }
                    else
                    {
                        foreach (string s in ex_delta.RightZ)
                            res_right.Add(s);
                    }
                    this.Z.Pop();
                    res_right.Reverse();
                    foreach (string term in res_right)
                        this.Z.Push(term);
                    res_right.Clear();
                }
                if (Z.Count == 0)
                    return "не принадлежит языку";
                if (Z.Peek().ToString() == "e")//в стеке видим эпсилон
                {
                    if (str[i].ToString() == "e")
                        return arrToStr(translated);
                    else
                        Z.Pop();
                }
            } // end for
            return "ошибка цикла";
        }

        public void Add_extended_Delta(string LeftQ, string LeftT, string LeftZ, ArrayList RightQ, ArrayList RightZ, ArrayList RightNew)
        {
            DeltaList.Add(new DeltaOsimbol(LeftQ, LeftT, LeftZ, RightQ, RightZ, RightNew));
        }
        //печать дельта правил
        public override void debugDelta()
        {
            Console.WriteLine("Extended Deltarules :");
            if (this.DeltaList == null) { Console.WriteLine("null"); return; }         
            foreach (DeltaOsimbol delta in this.DeltaList)  {
              delta.debug();
            }
        }
        //поиск нужного дельта
        private DeltaOsimbol find_exDelta(string a)
        {
            foreach (DeltaOsimbol delta in this.DeltaList)
            {
                if (delta.LeftZ == a) return delta;
            }
            return null; //not find 
        }
        //проверка на принадлежность к выходному алфавиту
        public bool isPetta(string p) {
            foreach (string pi in this.Petta) {
                if (p == pi) return true;
            }
            return false;
        }
    }
}
