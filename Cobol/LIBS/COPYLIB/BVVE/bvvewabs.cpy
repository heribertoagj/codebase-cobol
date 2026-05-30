      *================================================================*
      *                    P R I M E I T                               *
      *----------------------------------------------------------------*
      *    BOOK........:   BVVEWABS                                    *
      *    ANALISTA....:   ADRIANA CAMARGO           - PRIMEIT         *
      *    TAMANHO.....:   027 BYTES                                   *
      *    DATA........:   05/02/2020                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   BLOCO DE CONTROLE PARA CHAMADAS A SERVICOS  *
      *                    FUNCIONAIS                                  *
      *----------------------------------------------------------------*
      *  DADOS DE RETORNO:                                             *
      *    BVVEWABS-R-COD-RETORNO      = CODIGO DE RETORNO             *
      *    BVVEWABS-R-COD-ERRO         = CODIGO DE ERRO                *
      *    BVVEWABS-R-COD-MENSAGEM     = CODIGO DE MENSAGEM            *
      *                                                                *
      *    TAMANHO RETORNO             = 014 BYTES                     *
      *----------------------------------------------------------------*
           05 BVVEWABS-HEADER.
               10 BVVEWABS-COD-LAYOUT  PIC X(008)   VALUE 'BVVEWABS'.
               10 BVVEWABS-TAM-LAYOUT  PIC 9(005)   VALUE      00027.
           05 BVVEWABS-REGISTRO.
               07  BVVEWABS-BLOCO-RETORNO.
                   10 BVVEWABS-R-COD-RETORNO        PIC  9(002).
                   10 BVVEWABS-R-COD-ERRO           PIC  X(004).
                   10 BVVEWABS-R-COD-MENSAGEM       PIC  X(008).
