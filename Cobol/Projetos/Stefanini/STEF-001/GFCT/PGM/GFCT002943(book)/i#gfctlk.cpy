      *****************************************************************
      * NOME DA INC - I#GFCTLK                                        *
      * DESCRICAO   - ADESAO COMPULSORIA - CANCELAMENTO               *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTLK-LL   - 1010                                            *
      * DATA        - FEVEREIRO/2006                                  *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTLK-SAIDA.
           03 GFCTLK-LL                            PIC S9(004) COMP.
           03 GFCTLK-ZZ                            PIC S9(004) COMP.
           03 GFCTLK-TRANSACAO                     PIC  X(009).
           03 GFCTLK-FUNCAO                        PIC  X(008).
           03 GFCTLK-DADOS-RST.
              05 GFCTLK-QTDE-OCOR                  PIC  9(003).
              05 GFCTLK-QTDE-TOT-REG               PIC  9(006).
              05 GFCTLK-FUNC-BDSCO                 PIC  X(007).
              05 GFCTLK-ERRO                       PIC  9(001).
              05 GFCTLK-COD-MSG-ERRO               PIC  9(007).
              05 GFCTLK-COD-SQL-ERRO               PIC  9(003).
              05 GFCTLK-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTLK-AGPTO                      PIC  9(003).
              05 GFCTLK-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(049).
           03 GFCTLK-OCORR                         OCCURS 05 TIMES.
              05 GFCTLK-SEQUENCIA                  PIC  9(009).
              05 GFCTLK-PACOTE                     PIC  9(005).
              05 GFCTLK-DESC-PACOTE                PIC  X(020).
ST2506*       05 GFCTLK-CPF-CNPJ                   PIC  9(009).
ST2506*       05 GFCTLK-FILIAL                     PIC  9(005).
ST2506        05 GFCTLK-CPF-CNPJ                   PIC  X(009).
ST2506        05 GFCTLK-FILIAL                     PIC  X(004).
              05 GFCTLK-CONTROLE                   PIC  9(002).
              05 GFCTLK-AGENCIA                    PIC  9(005).
              05 GFCTLK-CONTA                      PIC  9(013).
              05 GFCTLK-POSTO                      PIC  9(005).
              05 GFCTLK-SEGMENTO                   PIC  9(003).
              05 GFCTLK-MUNICIPIO                  PIC  9(007).
              05 GFCTLK-UF                         PIC  X(002).
              05 GFCTLK-DESC-NOME                  PIC  X(040).
              05 GFCTLK-DATA-INICIO-ANT            PIC  X(010).
              05 GFCTLK-DATA-FIM-ANT               PIC  X(010).
              05 GFCTLK-DATA-INICIO                PIC  X(010).
              05 GFCTLK-DATA-FIM                   PIC  X(010).
              05 GFCTLK-ERRO-OCOR                  PIC  9(001).
