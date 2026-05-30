      *----------------------------------------------------------------*
      *    BRQ                                                         *
      *----------------------------------------------------------------*
      *    CENTRO CUSTO:   DCOM - DESCONTO COMERCIAL                   *
      *    PROJETO.....:   REDISTRIBUICAO DE LIMITES NO MOBILE BANKING *
      *    PROGRAMA....:   DCOM9869                                    *
      *    BOOK........:   DCOMWEIC                                    *
      *    PROGRAMADOR.:   LUIS EDUARDO                                *
      *    DATA........:   15/05/2021                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   HEADER DE COMUNICACAO COM OS PERIFERICOS    *
      *----------------------------------------------------------------*
      * OBSERVACOES:                                                   *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      * ALTERACOES:                                                    *
      *                                                                *
      * DATA:    NOME:     DESCRICAO:                                  *
      * XX/XX/XX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                *
      *----------------------------------------------------------------*
      *------------------  DESCRICAO DOS CAMPOS  ----------------------*
      *      DCOMWEIC-FUNCAO             FUNCAO INPUT                  *
      *         DCOMWEIC-FUNC-NORM       NORMAL                        *
      *         DCOMWEIC-FUNC-NORM2      NORMAL (BRANCOS)              *
      *         DCOMWEIC-FUNC-ANUL       ANULACAO (USA TRX ANULACAO    *
      *         DCOMWEIC-FUNC-VALID      VALIDA CARTAO E ENVIA AO IMS  *
      *         DCOMWEIC-FUNC-CONS       APENAS VALIDA CARTAO          *
      *                                                                *
      *      DCOMWEIC-RCODE              * RETURN CODE                 *
      *         DCOMWEIC-RC-OK           ... COMUNICACAO OK            *
      *         DCOMWEIC-RC-ERRO         ... ERRO GENERICO             *
      *         DCOMWEIC-RC-INDISP       ... IMS INDISPONIVEL          *
      *         DCOMWEIC-RC-ANUL         ... FUNCAO DE ANULACAO        *
      *      DCOMWEIC-ID-PERIF           * IDENTIFICACAO DO PERIFERICO *
      *      DCOMWEIC-TEXT-LEN           * TAMANHO AREA DE ENVIO/RECEB *
      *                                (APENAS QDO "LINK" P/ IC31)     *
      *      DCOMWEIC-RESERVA            * RESERVA                     *
      *                                                                *
      *----------------------------------------------------------------*
           05  DCOMWEIC-REGISTRO.
               10  DCOMWEIC-FUNCAO                     PIC  X(01).
                   88 DCOMWEIC-FUNC-NORM               VALUE 'N'.
                   88 DCOMWEIC-FUNC-NORM2              VALUE SPACES.
                   88 DCOMWEIC-FUNC-ANUL               VALUE 'A'.
                   88 DCOMWEIC-FUNC-VALID              VALUE 'V'.
                   88 DCOMWEIC-FUNC-CONS               VALUE 'C'.
               10  DCOMWEIC-RCODE REDEFINES   DCOMWEIC-FUNCAO.
                   12 FILLER                           PIC  X(01).
                   88 DCOMWEIC-RC-OK                   VALUE 'O'.
                   88 DCOMWEIC-RC-ERRO                 VALUE 'E'.
                   88 DCOMWEIC-RC-INDISP               VALUE 'I'.
                   88 DCOMWEIC-RC-ANUL                 VALUE 'A'.

               10  DCOMWEIC-ID-PERIF                   PIC  X(03).
               10  DCOMWEIC-TEXT-LEN                   PIC S9(04) COMP.
               10  DCOMWEIC-RESERVA                    PIC  X(04).
      *----------------------------------------------------------------*
