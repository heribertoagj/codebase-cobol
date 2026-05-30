      *****************************************************************
      * NOME DA INC - I#GFCTPL                                        *
      * DESCRICAO   - SUSPENSAO DE TARIFA - COMBO ARGUMENTOS          *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTPL-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTPL-SAIDA.
           03 GFCTPL-LL                            PIC S9(004) COMP.
           03 GFCTPL-ZZ                            PIC S9(004) COMP.
           03 GFCTPL-TRANSACAO                     PIC  X(009).
           03 GFCTPL-FUNCAO                        PIC  X(008).
           03 GFCTPL-DADOS-RST.
              05 GFCTPL-QTDE-OCOR                  PIC  9(003).
              05 GFCTPL-QTDE-TOT-REG               PIC  9(006).
              05 GFCTPL-FUNC-BDSCO                 PIC  X(007).
              05 GFCTPL-FILTRO.
                 07 GFCTPL-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTPL-FILTRO-TARIFA           PIC  9(005).
              05 GFCTPL-PONTEIRO.
                 07 GFCTPL-PONT-AGPTO              PIC  9(003).
                 07 GFCTPL-PONT-TARIFA             PIC  9(005).
                 07 GFCTPL-PONT-SEQ                PIC  9(009).
              05 GFCTPL-ERRO                       PIC  9(001).
              05 GFCTPL-COD-MSG-ERRO               PIC  9(007).
              05 GFCTPL-COD-SQL-ERRO               PIC  9(003).
              05 GFCTPL-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTPL-FIM                        PIC  X(001).
           03 FILLER                               PIC  X(017).
           03 GFCTPL-OCORR                         OCCURS 10 TIMES.
              05 GFCTPL-SEQUENCIA                  PIC  9(009).
              05 GFCTPL-CPF-CNPJ                   PIC  9(009).
              05 GFCTPL-FILIAL                     PIC  9(005).
              05 GFCTPL-CONTROLE                   PIC  9(002).
              05 GFCTPL-AGENCIA                    PIC  9(005).
              05 GFCTPL-POSTO                      PIC  9(005).
              05 GFCTPL-MUNICIPIO                  PIC  9(007).
              05 GFCTPL-UF                         PIC  X(002).
              05 GFCTPL-DESC-NOME                  PIC  X(030).
              05 GFCTPL-DINIC                      PIC  X(010).
