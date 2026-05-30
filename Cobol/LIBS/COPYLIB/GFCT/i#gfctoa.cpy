      ******************************************************************
      * NOME DA INC - I#GFCTOA                                         *
      * DESCRICAO   - COMBO DE TIPO DE AGRUPAMENTO                     *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 24/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOA-SAIDA.
           03  GFCTOA-LL                            PIC S9(004) COMP.
           03  GFCTOA-ZZ                            PIC S9(004) COMP.
           03  GFCTOA-TRANSACAO                     PIC  X(009).
           03  GFCTOA-FUNCAO                        PIC  X(008).
           03  GFCTOA-FILTRO.
               07  GFCTOA-COD-DEPDC                 PIC  9(005).
               07  GFCTOA-CCTA-CLI                  PIC  9(013).
               07  GFCTOA-CSERVC-TARIF              PIC  9(005).
           03  GFCTOA-QTDE-OCOR                     PIC  9(003).
           03  GFCTOA-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTOA-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOA-PONTEIRO.
               05  GFCTOA-PONTEIRO-DEPDC            PIC  9(005).
               05  GFCTOA-PONTEIRO-CCTA             PIC  9(013).
               05  GFCTOA-PONTEIRO-TARIF            PIC  9(005).
               05  GFCTOA-PONTEIRO-AGRUP            PIC  9(003).
           03  GFCTOA-ERRO                          PIC  9(001).
           03  GFCTOA-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOA-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOA-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOA-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(053).
           03  GFCTOA-DADOS.
               05  FILLER                           OCCURS 10.
                   07  GFCTOA-COD-AGRUP             PIC  9(003).
                   07  GFCTOA-DESC-AGRUP            PIC  X(030).
                   07  GFCTOA-SEQUENCIA             PIC  9(009).
                   07  GFCTOA-DT-INIC-ADSAO         PIC  X(010).
                   07  GFCTOA-HINCL-EXCEC-ADSAO     PIC  X(026).
