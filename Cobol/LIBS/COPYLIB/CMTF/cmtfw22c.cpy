      ******************************************************************
      * NOME BOOK  : LAYOUT DAS TABELAS REFERENTES A UORG              *
      *              (GFCTB049+GFCTB050+GFCTB0B2+GFCTB0B3)             *
      * DATA       : MAR / 2017                                        *
      * EMPRESA    : CAPGEMINI                                         *
      * AUTOR      : DIEGO DE SOUZA                                    *
      * TAMANHO    : 100 BYTES                                         *
      ******************************************************************
      * ENTRADA:                                                       *
      * E-CONV-USUARIO.                                                *
      *   E-CONV-USER-ALFA = SETAR SE ORIGEM FOR USUARIO ALFANUMERICO*
      *   E-CONV-USER-NUM   = SETAR SE ORIGEM FOR USUARIO NUMERICO     *
      * E-USUARIO-ALFA     = USUARIO A SER CONVERTIDO                 *
      *                                                                *
      * E-CONV-TIPO-OPER    = TIPO DE OPERACAO                         *
      *   E-CTPO-OPER-GFCT  = SETAR SE ORIGEM FOR GFCT                 *
      *   E-CTPO-OPER-GOTF  = SETAR SE ORIGEM FOR GOTF                 *
      * E-CTPO-OPER-MOTVO   = CODIGO TIPO DE OPERACAO MOVIMENTO        *
      *                                                                *
      * E-CONV-MOVIMENTO                                               *
      *   E-CONV-TOPER-GFCT = SETAR SE ORIGEM FOR GFCT                 *
      *   E-CONV-TOPER-GOTF = SETAR SE ORIGEM FOR GOTF                 *
      * E-CMOTVO            = CODIGO MOTIVO                            *
      ******************************************************************
      * SAIDA.                                                         *
      * S-USUARIO-ALFA     = USUARIO ALFANUMERO                      *
      * S-USUARIO-NUM       = USUARIO NUMERICO                         *
      * S-CTPO-OPER-GFCT    = CODIGO TIPO OPERACAO MOVIMENTO - GFCT    *
      * S-CTPO-OPER-GOTF    = CODIGO TIPO OPERACAO MOVIMENTO - GOTF    *
      * S-CMOTVO-GFCT       = CODIGO MOTIVO - GFCT                     *
      * S-CMOTVO-GOTF       = CODIGO MOTIVO - GOTF                     *
      ******************************************************************
         05 CMTFW22C-HEADER.
            10 CMTFW22C-COD-LAYOUT         PIC  X(008) VALUE 'CMTFW22C'.
            10 CMTFW22C-TAM-LAYOUT         PIC  9(005) VALUE 0100.
      *
         05 CMTFW22C-ENTRADA.
            10 CMTFW22C-E-CONV-USUARIO       PIC  X(001)   VALUE SPACES.
               88 CMTFW22C-E-CONV-USER-ALFA  VALUE 'A'.
               88 CMTFW22C-E-CONV-USER-NUM   VALUE 'N'.
            10 CMTFW22C-E-USUARIO            PIC  X(009)   VALUE SPACES.
      *
            10 CMTFW22C-E-CONV-TIPO-OPER     PIC  X(004)   VALUE SPACES.
               88 CMTFW22C-E-CTPO-OPER-GFCT  VALUE 'GFCT'.
               88 CMTFW22C-E-CTPO-OPER-GOTF  VALUE 'GOTF'.
            10 CMTFW22C-E-CTPO-OPER-MOTVO    PIC  9(003)   VALUE ZEROS.
      *
            10 CMTFW22C-E-CONV-MOVIMENTO     PIC  X(004)   VALUE SPACES.
               88 CMTFW22C-E-CONV-TOPER-GFCT VALUE 'GFCT'.
               88 CMTFW22C-E-CONV-TOPER-GOTF VALUE 'GOTF'.
            10 CMTFW22C-E-CMOTVO             PIC  9(002)   VALUE ZEROS.
      *
         05 CMTFW22C-SAIDA.
            10 CMTFW22C-S-USUARIO-ALFA       PIC  X(009)   VALUE SPACES.
            10 CMTFW22C-S-USUARIO-NUM        PIC  9(009)   VALUE ZEROS.
            10 CMTFW22C-S-CTPO-OPER-GFCT     PIC  9(003)   VALUE ZEROS.
            10 CMTFW22C-S-CTPO-OPER-GOTF     PIC  9(003)   VALUE ZEROS.
            10 CMTFW22C-S-CMOTVO-GFCT        PIC  9(002)   VALUE ZEROS.
            10 CMTFW22C-S-CMOTVO-GOTF        PIC  9(002)   VALUE ZEROS.
      *
         05 FILLER                           PIC  X(036)   VALUE SPACES.
      *
