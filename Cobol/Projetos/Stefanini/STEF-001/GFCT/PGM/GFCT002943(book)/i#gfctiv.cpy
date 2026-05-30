      ******************************************************************
      * NOME DA INC - I#GFCTIV                                         *
      * DESCRICAO   - TEXT DE NOME DO TITULAR CPF/CNPJ                 *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 23/12/2005                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTIV-SAIDA.
           03  GFCTIV-LL                         PIC S9(004) COMP.
           03  GFCTIV-ZZ                         PIC S9(004) COMP.
           03  GFCTIV-TRANSACAO                  PIC  X(009).
           03  GFCTIV-FUNCAO                     PIC  X(008).
           03  GFCTIV-QTDE-OCOR                  PIC  9(003).
           03  GFCTIV-QTDE-TOT-REG               PIC  9(006).
           03  GFCTIV-FUNC-BDSCO                 PIC  X(007).
           03  GFCTIV-ERRO                       PIC  9(001).
           03  GFCTIV-COD-MSG-ERRO               PIC  9(007).
           03  GFCTIV-COD-SQL-ERRO               PIC  9(003).
           03  GFCTIV-DESC-MSG-ERRO              PIC  X(079).
           03  GFCTIV-FIM                        PIC  X(001).
ST2506*    03  GFCTIV-NRO-CPF-CNPJ               PIC  9(009).
ST2506*    03  GFCTIV-NRO-FILIAL                 PIC  9(005).
ST2506     03  GFCTIV-NRO-CPF-CNPJ               PIC  X(009).
ST2506     03  GFCTIV-NRO-FILIAL                 PIC  X(004).
           03  GFCTIV-CTRL-CPF-CNPJ              PIC  9(002).
           03  GFCTIV-DADOS.
               05  GFCTIV-NOME-CLIENTE           PIC  X(040).
ST2510     03  FILLER                            PIC  X(827).
