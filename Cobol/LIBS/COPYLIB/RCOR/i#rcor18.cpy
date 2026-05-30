      * Y2K IBM - 10/05/1999 - SEM INTERVENCAO
      *-- 14/FEV/97   ----------- VERSAO 1.0 -----------   IC30COMC --*
      *                                                               *
      *                                                               *
      *        HEADER COMUNICACAO PERIFERICOS X IMS (VIA APPC)        *
      *        ===============================================        *
      * ............................................................. *
      * .                                                           . *
      * .      Obs.: Ao efetuar alguma alteracao, nao se esqueca de . *
      * . alterar a dummy correspondente em ASSEMBLER (IC30#COM)    . *
      * .                                                           . *
      * ............................................................. *
      *                                                               *
      *------------------  DESCRICAO DOS CAMPOS  ---------------------*
      *      IC30-FUNCAO               FUNCAO INPUT                   *
      *         IC30-FUNC-NORM         NORMAL                         *
      *         IC30-FUNC-NORM2        NORMAL (BRANCOS)               *
      *         IC30-FUNC-ANUL         ANULACAO (USA TRX ANULACAO     *
      *         IC30-FUNC-VALID        VALIDA CARTAO E ENVIA AO IMS   *
      *         IC30-FUNC-CONS         APENAS VALIDA CARTAO           *
      *      IC30-RCODE                * RETURN CODE                  *
      *         IC30-RC-OK             ... COMUNICACAO OK             *
      *         IC30-RC-ERRO           ... ERRO GENERICO              *
      *         IC30-RC-INDISP         ... IMS INDISPONIVEL           *
      *         IC30-RC-ANUL           ... FUNCAO DE ANULACAO         *
      *      IC30-ID-PERIF             * IDENTIFICACAO DO PERIFERICO  *
      *      IC30-TEXT-LEN             * TAMANHO AREA DE ENVIO/RECEB  *
      *                                  (APENAS QDO "LINK" P/ IC31)  *
      *      IC30-RESERVA              * RESERVA                      *
      *                                                               *
      *---------------------------------------------------------------*

           05 IC30-COM.
                10 IC30-FUNCAO               PIC X(001).
                    88 IC30-FUNC-NORM         VALUE 'N'.
                    88 IC30-FUNC-NORM2        VALUE SPACES.
                    88 IC30-FUNC-ANUL         VALUE 'A'.
                    88 IC30-FUNC-VALID        VALUE 'V'.
                    88 IC30-FUNC-CONS         VALUE 'C'.
                10 IC30-RCODE REDEFINES   IC30-FUNCAO.
                    12 FILLER                  PIC X(001).
                    88 IC30-RC-ERRO           VALUE 'E'.
                    88 IC30-RC-INDISP         VALUE 'I'.
                    88 IC30-RC-ANUL           VALUE 'A'.

                10 IC30-ID-PERIF             PIC X(03).

                10 IC30-TEXT-LEN             PIC S9(04) COMP.

                10 IC30-RESERVA              PIC X(04).


      *--------------------- END IC30COMC  ---------------------------*
