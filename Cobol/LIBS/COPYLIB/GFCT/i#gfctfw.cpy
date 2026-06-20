      *****************************************************************
      * NOME DA INC - I#GFCTFW                                        *
      * DESCRICAO   - CANCELAMENTO ESTORNO AGRUPAMENTO                *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTFW-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTFW-SAIDA.
           03 GFCTFW-LL                            PIC S9(004) COMP.
           03 GFCTFW-ZZ                            PIC S9(004) COMP.
           03 GFCTFW-TRANSACAO                     PIC  X(009).
           03 GFCTFW-FUNCAO                        PIC  X(008).
           03 GFCTFW-DADOS-RST.
              05 GFCTFW-FUNC-BDSCO                 PIC  X(007).
              05 GFCTFW-TERMINAL                   PIC  X(008).
              05 GFCTFW-FIM                        PIC  X(001).
           03 GFCTFW-ERRO                          PIC  9(001).
           03 GFCTFW-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTFW-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTFW-DESC-MSG-ERRO                 PIC  X(079).
           03 FILLER                               PIC  X(132).
           03 GFCTFW-FIXO.
              05 GFCTFW-OPCAO                      PIC  9(001).
     ***** 1 - CONSULTA  2 - EFETIVACAO
              05 GFCTFW-CAGPTO                     PIC  9(003).
              05 GFCTFW-RAGPTO                     PIC  X(030).
              05 GFCTFW-CSERVC-TARIF               PIC  9(005).
              05 GFCTFW-RSERVC-TARIF               PIC  X(100).
              05 GFCTFW-HORA                       PIC  X(026).
              05 GFCTFW-DATA-OCORR                 PIC  X(010).
ST2506*       05 GFCTFW-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTFW-FILIAL                     PIC  9(005).
ST2506        05 GFCTFW-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTFW-FILIAL                     PIC  X(004).
              05 GFCTFW-CONTROLE                   PIC  X(002).
              05 GFCTFW-CAGENCIA                   PIC  9(005).
              05 GFCTFW-RAGENCIA                   PIC  X(030).
              05 GFCTFW-CCONTA                     PIC  9(013).
              05 GFCTFW-RCONTA                     PIC  X(050).
              05 GFCTFW-CPAB                       PIC  9(005).
              05 GFCTFW-RPAB                       PIC  X(030).
              05 GFCTFW-TP-PESSOA                  PIC  X(001).
              05 GFCTFW-CSEGMENTO                  PIC  9(003).
              05 GFCTFW-RSEGMENTO                  PIC  X(030).
              05 GFCTFW-DATA-DEBITO                PIC  X(010).
              05 GFCTFW-DATA-EFETIVACAO            PIC  X(010).
              05 GFCTFW-ESTORNO-TOTAL              PIC  X(001).
              05 GFCTFW-COPERACAO                  PIC  9(003).
              05 GFCTFW-ROPERACAO                  PIC  X(030).
              05 GFCTFW-CMOTIVO                    PIC  9(002).
              05 GFCTFW-RMOTIVO                    PIC  X(030).
              05 GFCTFW-QESTORNOS                  PIC  9(012).
              05 GFCTFW-QCANCELAMENTOS             PIC  9(012).
              05 GFCTFW-VAESTORNAR                 PIC  9(012)V9(02).
              05 GFCTFW-VACANCELAR                 PIC  9(012)V9(02).
              05 GFCTFW-COMENTARIO                 PIC  X(255).
