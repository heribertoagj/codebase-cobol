      ******************************************************************
      * NOME DA INC - GFCTWAK0                                         *
      * DESCRICAO   - COMUNICACAO WEB X IMS                 - SAIDA    *
      *               DEFERIMENTO DA FLEXIBILIZACAO POR AG./CTA.       *
      *               COMBO CONJUNTO SERVICOS                          *
      * TAMANHO     - 1010 - TOTAL                                     *
      * DATA        - 25.01.2010                                       *
      * RESPONSAVEL - CIBELE             - SONDAPROCWORK               *
      ******************************************************************
      *
       01  WAK0-SAIDA.
           05  WAK0-LL                 PIC S9(004) COMP.
           05  WAK0-ZZ                 PIC S9(004) COMP.
           05  WAK0-TRANSACAO          PIC  X(009).
           05  WAK0-FUNCAO             PIC  X(008).
           05  WAK0-DADOS-RST.
               10  WAK0-QTDE-OCOR      PIC  9(003).
               10  WAK0-QTDE-TOT-REG   PIC  9(006).
               10  WAK0-FUNC-BDSCO     PIC  X(007).
               10  WAK0-FILTRO.
                   15  WAK0-FLT-AGENCIA     PIC  9(005).
                   15  WAK0-FLT-CONTA       PIC  9(013).
               10  WAK0-PONTEIRO.
                   15  WAK0-PTR-QTDE-REGS   PIC  9(005).
           05  WAK0-ERRO                    PIC  9(001).
           05  WAK0-COD-MSG-ERRO            PIC  9(007).
           05  WAK0-COD-SQL-ERRO            PIC  9(003).
           05  WAK0-DESC-MSG-ERRO           PIC  X(079).
           05  WAK0-FIM                     PIC  X(001).
           05  FILLER                       PIC  X(166).
           05  WAK0-DADOS              OCCURS 21.
               10  WAK0-COD-CONJ-SERVC      PIC  9(003).
               10  WAK0-DESCR-CONJ-SERVC    PIC  X(030).
