      ******************************************************************
      * DESCRICAO  : OBTER PROXIMO NUMERO SEQUENCIAL PARA ELEMENTO E   *
      *              CHAVE INFORMADA                                   *
      * COPYBOOK   : DCOMWNSI - BOOK DE ACESSO AO PROGRAMA BASE        *
      * FUNCIONAL  : DCOM3NSI - OBTER PROXIMO NRO SEQUENCIAL ELEMENTO  *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWNSI-E-CELMTO-DESC-COML   = ELEMENTO DE DESCONTO           *
      * DCOMWNSI-E-WCHAVE-PRINC-ELMTO = CHAVE PRINCIPAL DO ELEMENTO    *
      * DCOMWNSI-E-QDIG-SEQ-ELMTO     = QUANTIDADE DE DIGITO PARA O    *
      *                                 O NUMERO SEQUENCIAL DO ELEMENTO*
      **                                                              **
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWNSI-S-NULT-SEQ-ELMTO     = NUMERO SEQUENCIAL OBTIDO       *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWNSI-HEADER.
             10 DCOMWNSI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWNSI'.
             10 DCOMWNSI-TAM-LAYOUT    PIC 9(005) VALUE 00043.
      *
          05 DCOMWNSI-REGISTRO.
             10 DCOMWNSI-BLOCO-ENTRADA.
                15 DCOMWNSI-E-CELMTO-DESC-COML    PIC  9(003).
                15 DCOMWNSI-E-WCHAVE-PRINC-ELMTO  PIC  9(009).
                15 DCOMWNSI-E-QDIG-SEQ-ELMTO      PIC  9(003).
      *
             10 DCOMWNSI-BLOCO-SAIDA.
                15 DCOMWNSI-S-NULT-SEQ-ELMTO      PIC  9(015).
