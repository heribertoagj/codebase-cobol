      ******************************************************************
      *    I#BVVERM - LAY-OUT DO ARQUIVO DE CONTRATOS CONTENDO O STATUS*
      *               DA RENOVACAO AUTOMATICA                          *
      *    LRECL: 300                                                  *
      ******************************************************************
       01  REG-PEDCTREN.
           03  PEDCTREN-CNPJ           PIC 9(008).
           03  PEDCTREN-FILIAL         PIC 9(004).
           03  PEDCTREN-CONTROLE       PIC 9(002).
           03  PEDCTREN-RAZAO-SOCIAL   PIC X(040).
           03  PEDCTREN-VLR-LIMITE     PIC 9(013).
           03  PEDCTREN-COD-PRODUTO    PIC 9(003).
           03  PEDCTREN-NRO-CONTRATO   PIC 9(011).
           03  PEDCTREN-DT-VENCIMENTO  PIC 9(008).
           03  PEDCTREN-STATUS         PIC X(001).
           03  FILLER                  PIC X(210).
