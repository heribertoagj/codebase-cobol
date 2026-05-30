      *****************************************************************
      * NOME DA INC - I#GFCTGI                                        *
      * DESCRICAO   - DEFERIMENTO ESTORNO AGRUPAMENTO                 *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTGI-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTGI-SAIDA.
           03 GFCTGI-LL                            PIC S9(004) COMP.
           03 GFCTGI-ZZ                            PIC S9(004) COMP.
           03 GFCTGI-TRANSACAO                     PIC  X(009).
           03 GFCTGI-FUNCAO                        PIC  X(008).
           03 GFCTGI-DADOS-RST.
              05 GFCTGI-QTDE-OCOR                  PIC  9(003).
              05 GFCTGI-QTDE-TOT-REG               PIC  9(006).
              05 GFCTGI-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGI-FILTRO.
                 07 GFCTGI-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTGI-FILTRO-TARIFA           PIC  9(005).
              05 GFCTGI-PONTEIRO.
                 07 GFCTGI-PONT-AGPTO              PIC  9(003).
                 07 GFCTGI-PONT-TARIFA             PIC  9(005).
                 07 GFCTGI-PONT-DATA               PIC  X(010).
                 07 GFCTGI-PONT-TIMESTAMP          PIC  X(026).
           03 GFCTGI-ERRO                          PIC  9(001).
           03 GFCTGI-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTGI-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTGI-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTGI-FIM                           PIC  X(001).
           03 FILLER                               PIC  X(110).
           03 GFCTGI-OCORR                         OCCURS 04 TIMES.
ST2507*       05 GFCTGI-CPF-CNPJ                   PIC  9(009).
ST2507*       05 GFCTGI-FILIAL                     PIC  9(005).
ST2507        05 GFCTGI-CPF-CNPJ                   PIC  X(009).
ST2507        05 GFCTGI-FILIAL                     PIC  X(004).
              05 GFCTGI-CONTROLE                   PIC  9(002).
              05 GFCTGI-AGENCIA                    PIC  9(005).
              05 GFCTGI-CONTA                      PIC  9(013).
              05 GFCTGI-POSTO                      PIC  9(005).
              05 GFCTGI-SEGMENTO                   PIC  9(003).
              05 GFCTGI-DESC-NOME                  PIC  X(040).
              05 GFCTGI-DATA                       PIC  X(010).
              05 GFCTGI-TARIFA                     PIC  9(005).
              05 GFCTGI-DESC-TARIFA                PIC  X(040).
              05 GFCTGI-VALOR                      PIC  9(009)V9(002).
              05 GFCTGI-SIT                        PIC  9(001).
              05 GFCTGI-DEP                        PIC  9(005).
              05 GFCTGI-HORA                       PIC  X(026).
