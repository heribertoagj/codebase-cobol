      ******************************************************************
      * DESCRICAO  : VERIFICAR GRADE HORARIO DO SISTEMA DCOM           *
      * COPYBOOK   : DCOMWAAS - BOOK DE SAIDA                          *
      * COORDENADOR: DCOM1AAC - CONSULTAR GRADE HORARIO                *
      * FLUXO      : DCOMIAAS                                          *
      * DATA       : 01/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWAAS-SITUACAO-GRADE                                        *
      *                       = SITUACAO DA GRADE HORARIA              *
      *                          01=DISPONIVEL                         *
      *                          02=INDISPONIVEL                       *
      *                          03=INDISPONIVEL PARA O RETROATIVO     *
      *                          04=INDISPONIVEL PARA ULTIMO DIA DO ANO*
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWAAS-HEADER.
             10 DCOMWAAS-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAAS'.
             10 DCOMWAAS-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWAAS-REGISTRO.
             10 DCOMWAAS-BLOCO-SAIDA.
                15 DCOMWAAS-SITUACAO-GRADE        PIC  9(002).
                15 DCOMWAAS-RESERVA               PIC  X(085).
