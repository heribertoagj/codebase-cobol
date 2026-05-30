      **************************** GSEGW508 ****************************
      **                                                              **
      **    INCLUDE : GSEGW508                                        **
      **       DATA : NOVEMBRO/2010                                   **
      **  UTILIDADE : COMMAREA PARA CHAMADA DA API GSEG1508           **
      **              . CONVERTE CODIGO USUARIO RACF EM FUNCIONAL     **
      **              . CONVERTE FUNCIONAL EM CODIGO USUARIO RACF     **
      **      AUTOR : CPMBRAXIS                                       **
      **                                                              **
      ******************************************************************
      **                                                              **
      ** BLOCO HEADER                                                 **
      **  COD-LAYOUT                 IDENTIFICADOR DO LAYOUT          **
      **  TAM-LAYOUT                 TAMANHO DO LAYOUT                **
      **                                                              **
      ** BLOCO DE RETORNO                                             **
      **  COD-RETORNO                CODIGO DE RETORNO DA EXECUCAO:   **
      **                             00 - EXECUCAO OK                 **
      **                             08 - EXECUCAO NAO OK COM ALGUM   **
      **                                  ERRO CONTROLADO             **
      **                             12 - EXECUCAO NAO OK COM ALGUM   **
      **                                  ERRO NAO CONTROLADO         **
      **                             16 - EXECUCAO NAO OK COM ALGUM   **
      **                                  ERRO FATAL                  **
      **  COD-ERRO                   CODIGO IDENTIFICADOR DO ERRO     **
      **  COD-MENSAGEM               CODIGO IDENTIFICADOR DA MENSA-   **
      **                             GEM QUE SERA RETORNADA PARA O    **
      **                             PROGRAMA CHAMADOR                **
      **                                                              **
      ** BLOCO DE ENTRADA                                             **
      **  VERSAO                     VERSAO DO SERVICO (FIXO 001)     **
      **  FUNCAO                     FUNCAO A SER EXECUTADA:          **
      **                             01 - CONVERTER CODIGO USUARIO    **
      **                                  RACF EM FUNCIONAL           **
      **                             02 - CONVERTER FUNCIONAL EM      **
      **                                  CODIGO USUARIO RACF         **
      **  COD-USUARIO-ENT            CODIGO DO USUARIO GSEG (RACF).   **
      **                             ESTE CAMPO DEVE SER PREENCHIDO   **
      **                             SOMENTE PARA FUNCAO = 01.        **
      **  FUNCIONAL-ENT              CODIGO FUNCIONAL DO USUARIO      **
      **                             ESTE CAMPO DEVE SER PREENCHIDO   **
      **                             SOMENTE PARA FUNCAO = 02.        **
      **                                                              **
      ** BLOCO DE SAIDA                                               **
      **  COD-USUARIO-SAI            CODIGO DO USUARIO GSEG (RACF)    **
      **                             ESTE CAMPO SERA RETORNADO        **
      **                             SOMENTE PARA FUNCAO = 02.        **
      **  FUNCIONAL-SAI              CODIGO FUNCIONAL DO USUARIO      **
      **                             ESTE CAMPO SERA RETORNADO        **
      **                             SOMENTE PARA FUNCAO = 01.        **
      **                                                              **
      ******************************************************************
      *
           05  GSEGW508-HEADER.
      *
               10  GSEGW508-COD-LAYOUT                 PIC X(008)
                                                       VALUE 'GSEGW508'.
      *
               10  GSEGW508-TAM-LAYOUT                 PIC 9(005)
                                                       VALUE 68.
      *
           05  GSEGW508-REGISTRO.
      *
               10  GSEGW508-BLOCO-RETORNO.
      *
                   15  GSEGW508-COD-RETORNO            PIC 9(002).
                       88 GSEGW508-EXECOK              VALUE 00.
                       88 GSEGW508-EXECNOK-ERRO-ESP    VALUE 08.
                       88 GSEGW508-EXECNOK-ERRO-NESP   VALUE 12.
                       88 GSEGW508-EXECNOK-ERRO-FATAL  VALUE 16.
      *
                   15  GSEGW508-COD-ERRO               PIC X(004).
      *
                   15  GSEGW508-COD-MENSAGEM           PIC X(008).
      *
               10  GSEGW508-BLOCO-ENTRADA.
      *
                   15  GSEGW508-VERSAO                 PIC 9(003).
      *
                   15  GSEGW508-FUNCAO                 PIC 9(002).
      *
                   15  GSEGW508-COD-USUARIO-ENT        PIC X(009).
      *
                   15  GSEGW508-FUNCIONAL-ENT          PIC 9(009).
      *
               10  GSEGW508-BLOCO-SAIDA.
      *
                   15  GSEGW508-COD-USUARIO-SAI        PIC X(009).
      *
                   15  GSEGW508-FUNCIONAL-SAI          PIC 9(009).
      *
      **************************** GSEGW508 ****************************
