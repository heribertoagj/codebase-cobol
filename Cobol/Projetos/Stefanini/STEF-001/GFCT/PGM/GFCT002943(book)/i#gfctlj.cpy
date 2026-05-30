      *****************************************************************
      * NOME DA INC - I#GFCTLJ                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - CANCELAMENTO               *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTLJ-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01 GFCTLJ-ENTRADA.
           03  GFCTLJ-LL                            PIC S9(004) COMP.
           03  GFCTLJ-ZZ                            PIC S9(004) COMP.
           03  GFCTLJ-TRANSACAO                     PIC  X(009).
           03  GFCTLJ-FUNCAO                        PIC  X(008).
           03  GFCTLJ-DADOS-RST.
              05  GFCTLJ-QTDE-OCOR                  PIC  9(003).
              05  GFCTLJ-QTDE-TOT-REG               PIC  9(006).
              05  GFCTLJ-FUNC-BDSCO                 PIC  X(007).
              05  GFCTLJ-AGPTO                      PIC  9(003).
              05  GFCTLJ-FIM                        PIC  X(001).
              05  FILLER                            PIC  X(139).
           03 GFCTLJ-OCORR                         OCCURS 05 TIMES.
              05 GFCTLJ-SEQUENCIA                  PIC  9(009).
              05 GFCTLJ-PACOTE                     PIC  9(005).
              05 GFCTLJ-DESC-PACOTE                PIC  X(020).
ST2506*       05  GFCTLJ-CPF-CNPJ                   PIC  9(009).
ST2506*       05  GFCTLJ-FILIAL                     PIC  9(005).
ST2506        05  GFCTLJ-CPF-CNPJ                   PIC  X(009).
ST2506        05  GFCTLJ-FILIAL                     PIC  X(004).
              05  GFCTLJ-CONTROLE                   PIC  9(002).
              05  GFCTLJ-AGENCIA                    PIC  9(005).
              05  GFCTLJ-CONTA                      PIC  9(013).
              05  GFCTLJ-POSTO                      PIC  9(005).
              05  GFCTLJ-SEGMENTO                   PIC  9(003).
              05  GFCTLJ-MUNICIPIO                  PIC  9(007).
              05  GFCTLJ-UF                         PIC  X(002).
              05  GFCTLJ-DESC-NOME                  PIC  X(040).
              05  GFCTLJ-DATA-INICIO-ANT            PIC  X(010).
              05  GFCTLJ-DATA-FIM-ANT               PIC  X(010).
              05  GFCTLJ-DATA-INICIO                PIC  X(010).
              05  GFCTLJ-DATA-FIM                   PIC  X(010).
              05  GFCTLJ-ERRO-OCOR                  PIC  9(001).
