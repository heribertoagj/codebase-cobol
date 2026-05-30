      *---------------------------------------------------------------*
      *   NOME INC             -DDNAME-                LENGTH  0180   *
      *   I#DCOMBV              ------                 SISTEMA DCOM   *
      *---------------------------------------------------------------*
      *                                                               *
      *   DESCRICAO: LAYOUT DA TRANSFERENCIA DE DOMICILIO BANCARIO    *
      *                                                               *
      *   PROPOSTA DDS - 07/0208.                                     *
      *---------------------------------------------------------------*

       01  DCOMBV-REG.
            05 BV-RET-COD-ENVIO               PIC 9(02).
            05 BV-RET-CCUSTO                  PIC X(04).
            05 BV-DATA-MOVIMENTO              PIC 9(08).
            05 BV-ENT-COD-OPERADORA           PIC 9(02).
            05 BV-ENT-TIPO-PRODUTO            PIC X(01).
            05 BV-ENT-NOME-OPERADORA          PIC X(30).
      ************************************************
      *****IDENTIFICACAO - ANTERIOR (DE)**************
      ************************************************
            05 BV-ENT-IDENT-BANCO-ANT         PIC 9(05).
            05 BV-ENT-IDENT-AGENCIA-ANT       PIC 9(05).
            05 BV-ENT-IDENT-CONTA-ANT         PIC 9(13).
      *************************************************
      ******NOVA IDENTIFICACAO (PARA)    **************
      *************************************************
            05 BV-ENT-IDENT-BANCO-NOVA        PIC 9(05).
            05 BV-ENT-IDENT-AGENCIA-NOVA      PIC 9(05).
            05 BV-ENT-IDENT-CONTA-ATUAL       PIC 9(13).
      *************************************************
      ****** DADOS DE RETORNO            **************
      *************************************************
            05 BV-RET-COD-RETORNO             PIC 9(02).
            05 BV-RET-COD-MSG-INCONSISTE      PIC 9(03).
      ************************************************
      *****DADOS DA OPERACAO TRANSFERIDA**************
      ************************************************
            05 BV-ANO-OPERACAO                PIC 9(04).
            05 BV-NRO-SEQ-OPERACAO            PIC 9(09).
            05 BV-QTDE-PARC-VENCIDAS          PIC 9(05).
            05 BV-VLR-PARC-VENCIDAS           PIC 9(15)V99.
            05 BV-QTDE-PARC-AVENCER           PIC 9(05).
            05 BV-VLR-PARC-AVENCER            PIC 9(15)V99.
            05 BV-DT-MENOR-VCTO               PIC 9(08).
            05 BV-DT-MAIOR-VCTO               PIC 9(08).
            05 BV-RESERVA                     PIC X(09).
