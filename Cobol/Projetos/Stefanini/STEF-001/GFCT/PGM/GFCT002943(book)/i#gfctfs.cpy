      *****************************************************************
      * NOME DA INC - I#GFCTFS                                        *
      * DESCRICAO   - SOLICITACAO DE ESTORNO AGRUPAMENTO              *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTFS-LL   - 1010                                            *
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
       01  GFCTFS-SAIDA.
           03 GFCTFS-LL                            PIC S9(004) COMP.
           03 GFCTFS-ZZ                            PIC S9(004) COMP.
           03 GFCTFS-TRANSACAO                     PIC  X(009).
           03 GFCTFS-FUNCAO                        PIC  X(008).
           03 GFCTFS-DADOS-RST.
              05 GFCTFS-FUNC-BDSCO                 PIC  X(007).
              05 GFCTFS-TERMINAL                   PIC  X(008).
              05 GFCTFS-FIM                        PIC  X(001).
           03 GFCTFS-ERRO                          PIC  9(001).
           03 GFCTFS-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTFS-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTFS-DESC-MSG-ERRO                 PIC  X(079).
           03 GFCTFS-FILLER                        PIC  X(223).
           03 GFCTFS-FIXO.
              05 GFCTFS-CAGPTO                     PIC  9(003).
              05 GFCTFS-RAGPTO                     PIC  X(030).
              05 GFCTFS-CSERVC-TARIF               PIC  9(005).
              05 GFCTFS-RSERVC-TARIF               PIC  X(100).
              05 GFCTFS-ESTORNO-TOTAL              PIC  X(001).
ST2507*       05 GFCTFS-CGC-CPF                    PIC  9(009).
ST2507*       05 GFCTFS-FILIAL                     PIC  9(005).
ST2507        05 GFCTFS-CGC-CPF                    PIC  X(009).
ST2507        05 GFCTFS-FILIAL                     PIC  X(004).
              05 GFCTFS-CONTROLE                   PIC  X(002).
              05 GFCTFS-CAGENCIA                   PIC  9(005).
              05 GFCTFS-RAGENCIA                   PIC  X(030).
              05 GFCTFS-CCONTA                     PIC  9(013).
              05 GFCTFS-RCONTA                     PIC  X(050).
              05 GFCTFS-CPAB                       PIC  9(005).
              05 GFCTFS-RPAB                       PIC  X(030).
              05 GFCTFS-RAZAO                      PIC  9(005).
              05 GFCTFS-LCTO                       PIC  9(003).
              05 GFCTFS-TP-PESSOA                  PIC  X(001).
              05 GFCTFS-CSEGMENTO                  PIC  9(003).
              05 GFCTFS-RSEGMENTO                  PIC  X(030).
              05 GFCTFS-DATA-DEBITO                PIC  X(010).
              05 GFCTFS-COPERACAO                  PIC  9(003).
              05 GFCTFS-ROPERACAO                  PIC  X(030).
              05 GFCTFS-CMOTIVO                    PIC  9(002).
              05 GFCTFS-RMOTIVO                    PIC  X(030).
              05 GFCTFS-COMENTARIO                 PIC  X(255).
