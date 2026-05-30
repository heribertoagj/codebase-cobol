      *****************************************************************
      * NOME DA INC - I#GFCTRS                                        *
      * DESCRICAO   - PROCESSO BATCH DA SOLICITACAO DE CANCELAMENTO   *
      *               DO MOVIMENTO, ESTORNO AGRUPAMENTO E AG/CONTA    *
      * DATA        - MAIO/2006                                       *
      * RESPONSAVEL - VINICIUS - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTRS-ENTRADA.
           03 GFCTRS-CH-MOVTO.
              05 GFCTRS-DT-ENVIO-MOVTO      PIC  X(010).
              05 GFCTRS-CD-ROTINA-ORIGEM    PIC  X(004).
              05 GFCTRS-CD-NUMERO-ARQUIVO   PIC S9(002)    COMP-3.
              05 GFCTRS-CD-SEQ-MOVTO        PIC S9(011)    COMP-3.
           03 GFCTRS-CD-ORIGEM-PEDIDO       PIC  9(003).
           03 GFCTRS-CD-TIPO-SOLTC-ESTRN    PIC  9(001).
           03 GFCTRS-TARIFA                 PIC S9(005)    COMP-3.
           03 GFCTRS-VALOR                  PIC S9(009)V99 COMP-3.
           03 GFCTRS-BANCO                  PIC  9(003).
           03 GFCTRS-AGENCIA                PIC S9(005)    COMP-3.
           03 GFCTRS-CONTA                  PIC S9(013)    COMP-3.
           03 GFCTRS-BANCO-ORIGEM           PIC  9(003).
           03 GFCTRS-AGENCIA-ORIGEM         PIC S9(005)    COMP-3.
           03 GFCTRS-DATA-EVENTO            PIC  X(010).
           03 GFCTRS-STATUS                 PIC  9(002).
           03 GFCTRS-CTPO-OPER-MOTVO        PIC  9(003).
           03 GFCTRS-CMOTVO-JUSTF           PIC  9(002).
      *   STATUS 4-COBRADO PARCIAL
      *          5-COBRADO TOTAL
      *          6-ESTORNADO PARCIAL
           03 GFCTRS-CAGPTO                 PIC  9(003).
           03 GFCTRS-DATA-DEBITO            PIC  X(010).
           03 FILLER                        PIC  X(027).
