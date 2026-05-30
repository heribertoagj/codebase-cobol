      *****************************************************************
      * NOME DA INC - I#GFCTPB                                        *
      * DESCRICAO   - SUSPENSAO DE TARIFA - CONSULTA                  *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTPB-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTPB-SAIDA.
           03 GFCTPB-LL                            PIC S9(004) COMP.
           03 GFCTPB-ZZ                            PIC S9(004) COMP.
           03 GFCTPB-TRANSACAO                     PIC  X(009).
           03 GFCTPB-FUNCAO                        PIC  X(008).
           03 GFCTPB-DADOS-RST.
              05 GFCTPB-QTDE-OCOR                  PIC  9(003).
              05 GFCTPB-QTDE-TOT-REG               PIC  9(006).
              05 GFCTPB-FUNC-BDSCO                 PIC  X(007).
              05 GFCTPB-PONTEIRO.
                 07 GFCTPB-PONT-AGPTO              PIC  9(003).
                 07 GFCTPB-PONT-TARIFA             PIC  9(005).
                 07 GFCTPB-PONT-CH-NUM5            PIC  9(005).
                 07 GFCTPB-PONT-CH-NUM13           PIC  9(013).
                 07 GFCTPB-PONT-CH-ALP             PIC  X(002).
                 07 GFCTPB-PONT-CH-DATA            PIC  X(010).
              05 GFCTPB-ERRO                       PIC  9(001).
              05 GFCTPB-COD-MSG-ERRO               PIC  9(007).
              05 GFCTPB-COD-SQL-ERRO               PIC  9(003).
              05 GFCTPB-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTPB-FIM                        PIC  X(001).
              05 GFCTPB-FILTRO-TARIFA              PIC  9(005).
           03 FILLER                               PIC  X(139).
           03 GFCTPB-OCORR                         OCCURS 05 TIMES.
              05 GFCTPB-SEQUENCIA                  PIC  9(009).
              05 GFCTPB-TARIFA                     PIC  9(005).
              05 GFCTPB-DESC-TARIFA                PIC  X(040).
ST2507*       05 GFCTPB-CPF-CNPJ                   PIC  9(009).
ST2507*       05 GFCTPB-FILIAL                     PIC  9(005).
ST2507        05 GFCTPB-CPF-CNPJ                   PIC  X(009).
ST2507        05 GFCTPB-FILIAL                     PIC  X(004).
              05 GFCTPB-CONTROLE                   PIC  9(002).
              05 GFCTPB-AGENCIA                    PIC  9(005).
              05 GFCTPB-POSTO                      PIC  9(005).
              05 GFCTPB-MUNICIPIO                  PIC  9(007).
              05 GFCTPB-UF                         PIC  X(002).
              05 GFCTPB-DESC-NOME                  PIC  X(040).
              05 GFCTPB-DATA-INICIO                PIC  X(010).
              05 GFCTPB-ATIVO                      PIC  X(001).
