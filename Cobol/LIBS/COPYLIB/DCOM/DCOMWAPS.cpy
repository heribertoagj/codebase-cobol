      *================================================================*
      * DESCRICAO  : INCLUIR PROPOSTA TEMPORARIA TFWEB                 *
      * COPYBOOK   : DCOMWAPS - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1API - INCLUIR SIMULACAO TFWEB                *
      * FLUXO      : DDCOMIAPI                                         *
      * DATA       : 27/03/2024                                        *
      * AUTOR      : EDGARD ALMEIDA                                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAPS-HEADER.
             10 DCOMWAPS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAPS'.
             10 DCOMWAPS-TAM-LAYOUT    PIC 9(005)   VALUE 729.

             10 DCOMWAPS-BLOCO-SAIDA.
                15 DCOMWAPS-NSMULA-OPER-DESC    PIC  9(009).
                15 FILLER                       PIC  X(707).
