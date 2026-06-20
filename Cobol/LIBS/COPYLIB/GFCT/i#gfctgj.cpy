      *****************************************************************
      * NOME DA INC - I#GFCTGJ                                        *
      * DESCRICAO   - LISTA-EFETIVA DEFERIMENTO ESTORNO AGRUPAMENTO   *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTGJ-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTGJ-ENTRADA.
           03 GFCTGJ-LL                            PIC S9(004) COMP.
           03 GFCTGJ-ZZ                            PIC S9(004) COMP.
           03 GFCTGJ-TRANSACAO                     PIC  X(009).
           03 GFCTGJ-FUNCAO                        PIC  X(008).
           03 GFCTGJ-DADOS-RST.
              05 GFCTGJ-FUNC-BDSCO                 PIC  X(007).
              05 GFCTGJ-TERMINAL                   PIC  X(008).
              05 GFCTGJ-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(111).
           03 GFCTGJ-FIXO.
              05 GFCTGJ-OPCAO                      PIC  9(001).
     ***** 1 - CONSULTA  2 - EFETIVACAO
              05 GFCTGJ-CAGPTO                     PIC  9(003).
              05 GFCTGJ-CSERVC-TARIF               PIC  9(005).
              05 GFCTGJ-HORA                       PIC  X(026).
              05 GFCTGJ-DATA-EVENTO                PIC  X(010).
ST2506*       05 GFCTGJ-CGC-CPF                    PIC  9(009).
ST2506*       05 GFCTGJ-FILIAL                     PIC  9(005).
ST2506        05 GFCTGJ-CGC-CPF                    PIC  X(009).
ST2506        05 GFCTGJ-FILIAL                     PIC  X(004).
              05 GFCTGJ-CONTROLE                   PIC  X(002).
              05 GFCTGJ-CAGENCIA                   PIC  9(005).
              05 GFCTGJ-RAGENCIA                   PIC  X(030).
              05 GFCTGJ-CCONTA                     PIC  9(013).
              05 GFCTGJ-RCONTA                     PIC  X(050).
              05 GFCTGJ-CPAB                       PIC  9(005).
              05 GFCTGJ-RPAB                       PIC  X(030).
              05 GFCTGJ-CSEGMENTO                  PIC  9(003).
              05 GFCTGJ-RSEGMENTO                  PIC  X(030).
              05 GFCTGJ-DATA-SOLICITACAO           PIC  X(010).
              05 GFCTGJ-ESTORNO-TOTAL              PIC  X(001).
              05 GFCTGJ-COPERACAO                  PIC  9(003).
              05 GFCTGJ-ROPERACAO                  PIC  X(030).
              05 GFCTGJ-CMOTIVO                    PIC  9(002).
              05 GFCTGJ-RMOTIVO                    PIC  X(030).
              05 GFCTGJ-COMENTARIO                 PIC  X(255).
              05 GFCTGJ-QESTORNOS                  PIC  9(011).
              05 GFCTGJ-QCANCELAMENTOS             PIC  9(011).
              05 GFCTGJ-VAESTORNAR                 PIC  9(011)V9(02).
              05 GFCTGJ-VACANCELAR                 PIC  9(011)V9(02).
              05 GFCTGJ-PARECER                    PIC  9(001).
 ***********   2 - DEFERIDO      3 - INDEFERIDO
              05 GFCTGJ-JUST                       PIC  X(255).
