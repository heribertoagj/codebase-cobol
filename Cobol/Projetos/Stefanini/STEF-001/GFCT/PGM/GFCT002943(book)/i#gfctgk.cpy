      *****************************************************************
      * NOME DA INC - I#GFCTGK                                        *
      * DESCRICAO   - CANCELAMENTO ESTORNO AGRUPAMENTO                *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTGK-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTGK-SAIDA.
           03 GFCTGK-LL                            PIC S9(004) COMP.
           03 GFCTGK-ZZ                            PIC S9(004) COMP.
           03 GFCTGK-TRANSACAO                     PIC  X(009).
           03 GFCTGK-FUNCAO                        PIC  X(008).
           03 GFCTGK-DADOS-RST.
              05 GFCTGK-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGK-TERMINAL                   PIC  X(008).
              05 GFCTGK-FIM                        PIC  X(001).
           03 GFCTGK-ERRO                          PIC  9(001).
           03 GFCTGK-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTGK-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTGK-DESC-MSG-ERRO                 PIC  X(079).
           03 FILLER                               PIC  X(021).
           03 GFCTGK-FIXO.
              05 GFCTGK-OPCAO                      PIC  9(001).
     ***** 1 - CONSULTA  2 - EFETIVACAO
              05 GFCTGK-CAGPTO                     PIC  9(003).
              05 GFCTGK-CSERVC-TARIF               PIC  9(005).
              05 GFCTGK-HORA                       PIC  X(026).
              05 GFCTGK-DATA-EVENTO                PIC  X(010).
ST2506*       05 GFCTGK-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTGK-FILIAL                     PIC  9(005).
ST2506        05 GFCTGK-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTGK-FILIAL                     PIC  X(004).
              05 GFCTGK-CONTROLE                   PIC  X(002).
              05 GFCTGK-CAGENCIA                   PIC  9(005).
              05 GFCTGK-RAGENCIA                   PIC  X(030).
              05 GFCTGK-CCONTA                     PIC  9(013).
              05 GFCTGK-RCONTA                     PIC  X(050).
              05 GFCTGK-CPAB                       PIC  9(005).
              05 GFCTGK-RPAB                       PIC  X(030).
              05 GFCTGK-CSEGMENTO                  PIC  9(003).
              05 GFCTGK-RSEGMENTO                  PIC  X(030).
              05 GFCTGK-DATA-SOLICITACAO           PIC  X(010).
              05 GFCTGK-ESTORNO-TOTAL              PIC  X(001).
              05 GFCTGK-COPERACAO                  PIC  9(003).
              05 GFCTGK-ROPERACAO                  PIC  X(030).
              05 GFCTGK-CMOTIVO                    PIC  9(002).
              05 GFCTGK-RMOTIVO                    PIC  X(030).
              05 GFCTGK-COMENTARIO                 PIC  X(255).
              05 GFCTGK-QESTORNOS                  PIC  9(011).
              05 GFCTGK-QCANCELAMENTOS             PIC  9(011).
              05 GFCTGK-VAESTORNAR                 PIC  9(011)V9(02).
              05 GFCTGK-VACANCELAR                 PIC  9(011)V9(02).
              05 GFCTGK-PARECER                    PIC  9(001).
 ***********   2 - DEFERIDO      3 - INDEFERIDO
              05 GFCTGK-JUST                       PIC  X(255).
