      *****************************************************************
      * NOME DA INC - I#GFCTGO                                        *
      * DESCRICAO   - LISTA ESTORNO AGRUPAMENTO                       *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTGO-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTGO-SAIDA.
           03 GFCTGO-LL                            PIC S9(004) COMP.
           03 GFCTGO-ZZ                            PIC S9(004) COMP.
           03 GFCTGO-TRANSACAO                     PIC  X(009).
           03 GFCTGO-FUNCAO                        PIC  X(008).
           03 GFCTGO-DADOS-RST.
              05 GFCTGO-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGO-TERMINAL                   PIC  X(008).
              05 GFCTGO-FIM                        PIC  X(001).
           03 GFCTGO-ERRO                          PIC  9(001).
           03 GFCTGO-COD-MSG-ERRO                  PIC  9(007).
           03 GFCTGO-COD-SQL-ERRO                  PIC  9(003).
           03 GFCTGO-DESC-MSG-ERRO                 PIC  X(079).
           03 FILLER                               PIC  X(021).
           03 GFCTGO-FIXO.
              05 GFCTGO-CAGPTO                     PIC  9(003).
              05 GFCTGO-CSERVC-TARIF               PIC  9(005).
              05 GFCTGO-HORA                       PIC  X(026).
              05 GFCTGO-DATA-EVENTO                PIC  X(010).
ST2506*       05 GFCTGO-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTGO-FILIAL                     PIC  9(005).
ST2506        05 GFCTGO-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTGO-FILIAL                     PIC  X(004).
              05 GFCTGO-CONTROLE                   PIC  X(002).
              05 GFCTGO-CAGENCIA                   PIC  9(005).
              05 GFCTGO-RAGENCIA                   PIC  X(030).
              05 GFCTGO-CCONTA                     PIC  9(013).
              05 GFCTGO-RCONTA                     PIC  X(050).
              05 GFCTGO-CPAB                       PIC  9(005).
              05 GFCTGO-RPAB                       PIC  X(030).
              05 GFCTGO-CSEGMENTO                  PIC  9(003).
              05 GFCTGO-RSEGMENTO                  PIC  X(030).
              05 GFCTGO-DATA-SOLICITACAO           PIC  X(010).
              05 GFCTGO-ESTORNO-TOTAL              PIC  X(001).
              05 GFCTGO-COPERACAO                  PIC  9(003).
              05 GFCTGO-ROPERACAO                  PIC  X(030).
              05 GFCTGO-CMOTIVO                    PIC  9(002).
              05 GFCTGO-RMOTIVO                    PIC  X(030).
              05 GFCTGO-COMENTARIO                 PIC  X(255).
              05 GFCTGO-QESTORNOS                  PIC  9(011).
              05 GFCTGO-QCANCELAMENTOS             PIC  9(011).
              05 GFCTGO-VAESTORNAR                 PIC  9(011)V9(02).
              05 GFCTGO-VACANCELAR                 PIC  9(011)V9(02).
              05 GFCTGO-PARECER                    PIC  9(001).
************  1 - PENDENTE   2 - DEFERIDO      3 - INDEFERIDO
              05 GFCTGO-SITUACAO                   PIC  9(001).
************* 1 - A PROCESSAR  2 - CALCULADO  3 - PROCESSADO
************* 4 - EXCL-USUARIO 5 - EXCL-DATA
              05 GFCTGO-JUST                       PIC  X(255).
