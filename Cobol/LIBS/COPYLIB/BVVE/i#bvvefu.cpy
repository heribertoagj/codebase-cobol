      *================================================================*
      *  ARQUIVO DE CONTRATOS PARA RENEGOCIACAO DE GARANTIA            *
      *                                                                *
      *  I#BVVEFU - 550 BYTES                                          *
      *----------------------------------------------------------------*
      *  CONTRATO-LOTE : CONTRATO NOVO ENVIADO NAS OPERACOES EM LOTE   *
      *  CONTRATO-RENEG: CONTRATO RENEGOCIADO                          *
      *  ID-OPER-CIP   : ID DA OPERACAO CIP (USO BVVE)                 *
      *================================================================*
      *                                                                *
       01  BVVEFU-REGISTRO.
           05 BVVEFU-CONTRATO-LOTE        PIC X(019).
           05 BVVEFU-CONTRATO-RENEG       PIC X(019).
           05 BVVEFU-ID-OPER-CIP          PIC X(019).
           05 FILLER                      PIC X(493).
