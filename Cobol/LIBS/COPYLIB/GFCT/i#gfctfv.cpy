      *****************************************************************
      * NOME DA INC - I#GFCTFV                                        *
      * DESCRICAO   - CANCELAMENTO ESTORNO AGRUPAMENTO                *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTFV-LL   - 1010                                            *
      * DATA        - SETEMBRO/2005                                   *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTFV-ENTRADA.
           03 GFCTFV-LL                            PIC S9(004) COMP.
           03 GFCTFV-ZZ                            PIC S9(004) COMP.
           03 GFCTFV-TRANSACAO                     PIC  X(009).
           03 GFCTFV-FUNCAO                        PIC  X(008).
           03 GFCTFV-DADOS-RST.
              05 GFCTFV-FUNC-BDSCO                 PIC  X(007).
              05 GFCTFV-TERMINAL                   PIC  X(008).
              05 GFCTFV-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(222).
           03 GFCTFV-FIXO.
              05 GFCTFV-OPCAO                      PIC  9(001).
     ***** 1 - CONSULTA  2 - EFETIVACAO
              05 GFCTFV-CAGPTO                     PIC  9(003).
              05 GFCTFV-RAGPTO                     PIC  X(030).
              05 GFCTFV-CSERVC-TARIF               PIC  9(005).
              05 GFCTFV-RSERVC-TARIF               PIC  X(100).
              05 GFCTFV-HORA                       PIC  X(026).
              05 GFCTFV-DATA-OCORR                 PIC  X(010).
              05 GFCTFV-CGC-CPF                    PIC  9(009).
              05 GFCTFV-FILIAL                     PIC  9(005).
              05 GFCTFV-CONTROLE                   PIC  X(002).
              05 GFCTFV-CAGENCIA                   PIC  9(005).
              05 GFCTFV-RAGENCIA                   PIC  X(030).
              05 GFCTFV-CCONTA                     PIC  9(013).
              05 GFCTFV-RCONTA                     PIC  X(050).
              05 GFCTFV-CPAB                       PIC  9(005).
              05 GFCTFV-RPAB                       PIC  X(030).
              05 GFCTFV-TP-PESSOA                  PIC  X(001).
              05 GFCTFV-CSEGMENTO                  PIC  9(003).
              05 GFCTFV-RSEGMENTO                  PIC  X(030).
              05 GFCTFV-DATA-DEBITO                PIC  X(010).
              05 GFCTFV-DATA-EFETICACAO            PIC  X(010).
              05 GFCTFV-ESTORNO-TOTAL              PIC  X(001).
              05 GFCTFV-COPERACAO                  PIC  9(003).
              05 GFCTFV-ROPERACAO                  PIC  X(030).
              05 GFCTFV-CMOTIVO                    PIC  9(002).
              05 GFCTFV-RMOTIVO                    PIC  X(030).
              05 GFCTFV-QESTORNOS                  PIC  9(012).
              05 GFCTFV-QCANCELAMENTOS             PIC  9(012).
              05 GFCTFV-VAESTORNAR                 PIC  9(012)V9(02).
              05 GFCTFV-VACANCELAR                 PIC  9(012)V9(02).
              05 GFCTFV-COMENTARIO                 PIC  X(255).
