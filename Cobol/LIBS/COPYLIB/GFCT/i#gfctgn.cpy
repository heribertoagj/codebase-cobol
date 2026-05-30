      *****************************************************************
      * NOME DA INC - I#GFCTGN                                        *
      * DESCRICAO   - LISTA CONSULTA ESTORNO AGRUPAMENTO              *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTGN-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTGN-ENTRADA.
           03 GFCTGN-LL                            PIC S9(004) COMP.
           03 GFCTGN-ZZ                            PIC S9(004) COMP.
           03 GFCTGN-TRANSACAO                     PIC  X(009).
           03 GFCTGN-FUNCAO                        PIC  X(008).
           03 GFCTGN-DADOS-RST.
              05 GFCTGN-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGN-TERMINAL                   PIC  X(008).
              05 GFCTGN-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(111).
           03 GFCTGN-FIXO.
              05 GFCTGN-CAGPTO                     PIC  9(003).
              05 GFCTGN-CSERVC-TARIF               PIC  9(005).
              05 GFCTGN-HORA                       PIC  X(026).
              05 GFCTGN-DATA-EVENTO                PIC  X(010).
              05 GFCTGN-CGC-CPF                    PIC  9(009).
              05 GFCTGN-FILIAL                     PIC  9(005).
              05 GFCTGN-CONTROLE                   PIC  X(002).
              05 GFCTGN-CAGENCIA                   PIC  9(005).
              05 GFCTGN-RAGENCIA                   PIC  X(030).
              05 GFCTGN-CCONTA                     PIC  9(013).
              05 GFCTGN-RCONTA                     PIC  X(050).
              05 GFCTGN-CPAB                       PIC  9(005).
              05 GFCTGN-RPAB                       PIC  X(030).
              05 GFCTGN-CSEGMENTO                  PIC  9(003).
              05 GFCTGN-RSEGMENTO                  PIC  X(030).
              05 GFCTGN-DATA-SOLICITACAO           PIC  X(010).
              05 GFCTGN-ESTORNO-TOTAL              PIC  X(001).
              05 GFCTGN-COPERACAO                  PIC  9(003).
              05 GFCTGN-ROPERACAO                  PIC  X(030).
              05 GFCTGN-CMOTIVO                    PIC  9(002).
              05 GFCTGN-RMOTIVO                    PIC  X(030).
              05 GFCTGN-COMENTARIO                 PIC  X(255).
              05 GFCTGN-QESTORNOS                  PIC  9(011).
              05 GFCTGN-QCANCELAMENTOS             PIC  9(011).
              05 GFCTGN-VAESTORNAR                 PIC  9(011)V9(02).
              05 GFCTGN-VACANCELAR                 PIC  9(011)V9(02).
              05 GFCTGN-PARECER                    PIC  9(001).
************   1 - PENDENTE    2 - DEFERIDO   3 - INDEFERIDO
              05 GFCTGN-SITUACAO                   PIC  9(001).
************* 1 - A PROCESSAR  2 - CALCULADO  3 - PROCESSADO
************* 4 - EXCL-USUARIO 5 - EXCL-DATA
              05 GFCTGN-JUST                       PIC  X(255).
