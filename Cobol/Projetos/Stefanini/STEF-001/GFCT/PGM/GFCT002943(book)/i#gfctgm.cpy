      *****************************************************************
      * NOME DA INC - I#GFCTGM                                        *
      * DESCRICAO   - CONSULTA ESTORNO AGRUPAMENTO                    *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTGM-LL   - 1010                                            *
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
       01  GFCTGM-SAIDA.
           03 GFCTGM-LL                            PIC S9(004) COMP.
           03 GFCTGM-ZZ                            PIC S9(004) COMP.
           03 GFCTGM-TRANSACAO                     PIC  X(009).
           03 GFCTGM-FUNCAO                        PIC  X(008).
           03 GFCTGM-DADOS-RST.
              05 GFCTGM-QTDE-OCOR                  PIC  9(003).
              05 GFCTGM-QTDE-TOT-REG               PIC  9(006).
              05 GFCTGM-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGM-FILTRO.
                 07 GFCTGM-FILTRO-AGPTO            PIC  9(003).
                 07 GFCTGM-FILTRO-TARIFA           PIC  9(005).
              05 GFCTGM-PONTEIRO.
                 07 GFCTGM-PONT-AGPTO              PIC  9(003).
                 07 GFCTGM-PONT-TARIFA             PIC  9(005).
                 07 GFCTGM-PONT-DATA               PIC  X(010).
                 07 GFCTGM-PONT-TIMESTAMP          PIC  X(026).
           03 GFCTGM-ERRO                          PIC  9(001).
           03 GFCTGM-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTGM-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTGM-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTGM-FIM                           PIC  X(001).
           03 FILLER                               PIC  X(110).
           03 GFCTGM-OCORR                         OCCURS 04 TIMES.
ST2507*       05 GFCTGM-CPF-CNPJ                   PIC  9(009).
ST2507*       05 GFCTGM-FILIAL                     PIC  9(005).
ST2507        05 GFCTGM-CPF-CNPJ                   PIC  X(009).
ST2507        05 GFCTGM-FILIAL                     PIC  X(004).
              05 GFCTGM-CONTROLE                   PIC  9(002).
              05 GFCTGM-AGENCIA                    PIC  9(005).
              05 GFCTGM-CONTA                      PIC  9(013).
              05 GFCTGM-POSTO                      PIC  9(005).
              05 GFCTGM-SEGMENTO                   PIC  9(003).
              05 GFCTGM-DESC-NOME                  PIC  X(040).
              05 GFCTGM-DATA                       PIC  X(010).
              05 GFCTGM-TARIFA                     PIC  9(005).
              05 GFCTGM-DESC-TARIFA                PIC  X(040).
              05 GFCTGM-VALOR                      PIC  9(009)V9(002).
              05 GFCTGM-SIT                        PIC  9(001).
              05 GFCTGM-DEP                        PIC  9(005).
              05 GFCTGM-HORA                       PIC  X(026).
           03 FILLER                               PIC  X(004).
