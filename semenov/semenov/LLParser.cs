using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.Collections;

namespace MPTranslator
{
    class LLParser
    {
        private myGrammar G;
        private Stack<string> Stack;
        private DataTable Table;
        public string OutputConfigure = "";
        Hashtable FirstSet = new Hashtable();
        Hashtable FollowSet = new Hashtable();

        public LLParser(myGrammar grammar)
        {
            G = grammar;
            Table = new DataTable("ManagerTable");
            Stack = new Stack<string>();
            // Создадим таблицу синтаксического анализа для этой грамматики

            // Определим структуру таблицы
            Table.Columns.Add(new DataColumn("VT", typeof(String)));
            foreach (string termSymbol in G.T)
            {
                Table.Columns.Add(new DataColumn(termSymbol, typeof(Prule)));
            }
            Table.Columns.Add(new DataColumn("EoI", typeof(Prule))); // Epsilon
            ComputeFirstSets(grammar); // Вычисляем множество First
            ComputeFollowSets(grammar); // Вычисляем множество Follow
            for (int i = 0; i < grammar.V.Count; i++) // Рассмотрим последовательно все нетерминалы
            {
                DataRow workRow = Table.NewRow();
                workRow["VT"] = (string)grammar.V[i];
                List<Prule> rules = getRules((string)grammar.V[i]); // Получим все правила, соответствующие текущему нетерминалу
                foreach (var rule in rules)
                {
                    List<string> currFirstSet = First(rule.RightChain.Cast<string>().ToList());
                    foreach (var firstSymbol in currFirstSet)
                    {
                        if (firstSymbol != "")
                        {
                            // Добавить в таблицу
                            workRow[firstSymbol] = rule;
                        }
                        else
                        {
                            List<string> currFollowSet = Follow(rule.leftNoTerm);
                            foreach (var currFollowSymb in currFollowSet)
                            {
                                string currFollowSymbFix = (currFollowSymb == "") ? "EoI" : currFollowSymb;
                                workRow[currFollowSymbFix] = rule;
                            }
                        }
                    }
                }
                Table.Rows.Add(workRow);
            }
        }

        public bool Parse(string input)
        {
            Stack.Push("EoS"); // символ окончания входной последовательности
            Stack.Push(G.S0);
            int i = 0;
            string currInputSymbol = input[i].ToString();
            string currStackSymbol;
            do
            {
                currStackSymbol = Stack.Peek();
                if (G.T.Contains(currStackSymbol)) // в вершине стека находится терминал
                {
                    if (currInputSymbol == currStackSymbol) // распознанный символ равен вершине стека
                    {
                        // Извлечь из стека верхний элемент и распознать символ входной последовательности (ВЫБРОС)
                        Stack.Pop();
                        if (i != input.Length)
                        {
                            i++;
                        }
                        currInputSymbol = (i == input.Length) ? "EoI" : input[i].ToString();
                    }
                    else
                    {
                        // ERROR
                        return false;
                    }
                }
                else // если в вершине стека нетерминал
                {
                    DataRow custRows = Table.Select("VT = '" + currStackSymbol.Replace(@"'", @"''") + "'", null)[0];
                    if (!custRows.IsNull(currInputSymbol)) // в клетке[вершина стека, распознанный символ] таблицы разбора существует правило
                    {
                        //  извлечь из стека элемент и занести в стек все терминалы и нетерминалы найденного в таблице правила в стек в порядке обратном порядку их следования в правиле
                        Stack.Pop();
                        foreach (var chainSymbol in ((Prule)custRows[currInputSymbol]).RightChain.Cast<string>().Reverse().ToList())
                        {
                            if (chainSymbol != "")
                            {
                                Stack.Push(chainSymbol);
                            }
                        }
                        OutputConfigure += (((Prule)custRows[currInputSymbol]).Id);
                    }
                    else
                    {
                        // ERROR
                        return false;
                    }
                }
            } while (Stack.Peek() != "EoS"); // вершина стека не равна концу входной последовательности

            if (i != input.Length) // распознанный символ не равен концу входной последовательности
            {
                // ERROR
                return false;
            }

            return true;
        }

        public List<Prule> getRules(string noTermSymbol)
        {
            List<Prule> result = new List<Prule>();
            for (int i = 0; i < G.Prules.Count; ++i)
            {
                Prule currRule = (Prule)G.Prules[i];
                if (currRule.leftNoTerm == noTermSymbol)
                {
                    result.Add(currRule);
                }
            }
            return result;
        }

        public void ComputeFirstSets(myGrammar grammar)
        {
            for (int i = 0; i < grammar.T.Count; i++)
                FirstSet[(string)grammar.T[i]] = new HashSet<string>() { grammar.T[i].ToString() };   // FIRST[c] = {c}*/
            for (int i = 0; i < grammar.V.Count; i++)
                FirstSet[(string)grammar.V[i]] = new HashSet<string>();                     //First[x] = empty list
            bool changes = true;
            while (changes)
            {
                changes = false;
                for (int i = 0; i < grammar.Prules.Count; ++i)
                {
                    // Для каждого правила X-> Y0Y1…Yn
                    Prule currRule = (Prule)(grammar.Prules[i]);
                    string X = currRule.LeftNoTerm;
                    List<string> Y = currRule.rightChain.Cast<string>().ToList();
                    for (int k = 0; k < Y.Count; k++)
                    {
                        foreach (string currFirstSymb in (HashSet<string>)FirstSet[Y[k]])
                        {
                            if (((HashSet<string>)FirstSet[X]).Add(currFirstSymb)) //Добавить а в FirstSets[X]
                            {
                                changes = true;
                            }
                        }
                        if (!((HashSet<string>)FirstSet[Y[k]]).Contains(""))
                        {
                            break;
                        }
                    }
                }
            } //  пока вносятся изменения
        }
        public List<string> First(string X) { return ((HashSet<string>)FirstSet[X]).ToList(); }
        public List<string> First(List<string> Y)
        {
            List<string> result = new List<string>();
            for (int k = 0; k < Y.Count; k++)
            {
                foreach (string currFirstSymb in (HashSet<string>)FirstSet[Y[k]])
                {
                    result.Add(currFirstSymb);
                }
                if (!((HashSet<string>)FirstSet[Y[k]]).Contains(""))
                {
                    break;
                }
            }
            return result;
        }

        public void ComputeFollowSets(myGrammar grammar)
        {
            for (int i = 0; i < grammar.V.Count; i++)
                FollowSet[(string)grammar.V[i]] = new HashSet<string>();
            FollowSet[G.S0] = new HashSet<string>() { "" };
            bool changes = true;
            while (changes)
            {
                changes = false;
                for (int i = 0; i < grammar.Prules.Count; ++i)
                {
                    // Для каждого правила X-> Y0Y1…Yn
                    Prule currRule = (Prule)(grammar.Prules[i]);
                    List<string> rightChain = currRule.RightChain.Cast<string>().ToList();
                    for (int indexOfSymbol = 0; indexOfSymbol < rightChain.Count; ++indexOfSymbol)
                    {
                        string currSymbol = rightChain[indexOfSymbol];
                        if (G.T.Contains(currSymbol))
                        {
                            continue;
                        }
                        if (indexOfSymbol == rightChain.Count - 1)
                        {
                            foreach (string currFollowSymbol in (HashSet<string>)FollowSet[currRule.LeftNoTerm])
                            {
                                if (((HashSet<string>)FollowSet[rightChain[indexOfSymbol]]).Add(currFollowSymbol))
                                {
                                    changes = true;
                                }
                            }
                        }
                        else
                        {
                            List<string> currFirst = First(rightChain[indexOfSymbol + 1]);
                            bool epsFound = false;
                            foreach (var currFirstSymbol in currFirst)
                            {
                                if (currFirstSymbol != "")
                                {
                                    if (((HashSet<string>)FollowSet[rightChain[indexOfSymbol]]).Add(currFirstSymbol))
                                    {
                                        changes = true;
                                    }
                                }
                                else
                                {
                                    epsFound = true;
                                }
                            }
                            if (epsFound)
                            {
                                foreach (string currFollowSymbol in (HashSet<string>)FollowSet[currRule.LeftNoTerm])
                                {
                                    if (((HashSet<string>)FollowSet[rightChain[indexOfSymbol]]).Add(currFollowSymbol))
                                    {
                                        changes = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        public List<string> Follow(string X) { return ((HashSet<string>)FollowSet[X]).ToList(); }

    }
}