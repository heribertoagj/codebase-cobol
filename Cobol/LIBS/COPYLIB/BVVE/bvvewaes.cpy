      *================================================================*
      *                    P R I M E I T                               *
      *----------------------------------------------------------------*
      *    BOOK........:   BVVEWAES                                    *
      *    ANALISTA....:   ADRIANA CAMARGO           - PRIMEIT         *
      *    TAMANHO.....:   027 BYTES                                   *
      *    DATA........:   05/02/2020                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   BLOCO DE CONTROLE PARA CHAMADAS A SERVICOS  *
      *                    FUNCIONAIS                                  *
      *----------------------------------------------------------------*
      *  DADOS DE RETORNO:                                             *
      *    BVVEWAES-R-COD-RETORNO      = CODIGO DE RETORNO             *
      *    BVVEWAES-R-COD-ERRO         = CODIGO DE ERRO                *
      *    BVVEWAES-R-COD-MENSAGEM     = CODIGO DE MENSAGEM            *
      *                                                                *
      *    TAMANHO RETORNO             = 014 BYTES                     *
      *----------------------------------------------------------------*
           05 BVVEWAES-HEADER.
               10 BVVEWAES-COD-LAYOUT  PIC X(008)   VALUE 'BVVEWAES'.
               10 BVVEWAES-TAM-LAYOUT  PIC 9(005)   VALUE      00027.
           05 BVVEWAES-REGISTRO.
               07  BVVEWAES-BLOCO-RETORNO.
                   10 BVVEWAES-R-COD-RETORNO        PIC  9(002).
                   10 BVVEWAES-R-COD-ERRO           PIC  X(004).
                   10 BVVEWAES-R-COD-MENSAGEM       PIC  X(008).
