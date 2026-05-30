      ******************************************************************
      *                                                                *
      *    I#DCOMXH - AREA AUXILIAR DE REDEFINICAO DE VARIAVEIS PARA   *
      *               PADRONIZACAO DA FERRAMENTA HEXAVISION.           *
      *                                                                *
      *     LEGENDA :                                                  *
      *     ========                                                   *
      *                                                                *
      *  =>      HX - INDICA QUE A VARIAVEL EH UTILIZADA PARA VALIDA-  *
      *               CAO DO PROGRAMA PELA FERRAMENTA HEXAVISION.      *
      *  =>      CS - INDICA QUE A VARIAVEIS EH SINALIZADA.            *
      *  =>      SS - INDICA QUE A VARIAVEIS NAO EH SINALIZADA.        *
      *                                                                *
      ******************************************************************

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS SEM CASAS DECIMAIS.           *
      *----------------------------------------------------------------*

       01  HX-NUM-01-CS                PIC +9(001)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-01-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-01-SS            PIC  9(001).

       01  HX-NUM-02-CS                PIC +9(002)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-02-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-02-SS            PIC  9(002).

       01  HX-NUM-03-CS                PIC +9(003)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-03-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-03-SS            PIC  9(003).

       01  HX-NUM-04-CS                PIC +9(004)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-04-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-04-SS            PIC  9(004).

       01  HX-NUM-05-CS                PIC +9(005)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-05-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-05-SS            PIC  9(005).

       01  HX-NUM-06-CS                PIC +9(006)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-06-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-06-SS            PIC  9(006).

       01  HX-NUM-07-CS                PIC +9(007)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-07-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-07-SS            PIC  9(007).

       01  HX-NUM-08-CS                PIC +9(008)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-08-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-08-SS            PIC  9(008).

       01  HX-NUM-09-CS                PIC +9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-09-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-09-SS            PIC  9(009).

       01  HX-NUM-10-CS                PIC +9(010)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-10-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-10-SS            PIC  9(010).

       01  HX-NUM-11-CS                PIC +9(011)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-11-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-11-SS            PIC  9(011).

       01  HX-NUM-12-CS                PIC +9(012)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-12-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-12-SS            PIC  9(012).

       01  HX-NUM-13-CS                PIC +9(013)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-13-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-13-SS            PIC  9(013).

       01  HX-NUM-14-CS                PIC +9(014)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-14-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-14-SS            PIC  9(014).

       01  HX-NUM-15-CS                PIC +9(015)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-15-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-15-SS            PIC  9(015).

       01  HX-NUM-16-CS                PIC +9(016)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-16-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-16-SS            PIC  9(016).

       01  HX-NUM-17-CS                PIC +9(017)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-17-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-17-SS            PIC  9(017).

       01  HX-NUM-18-CS                PIC +9(018)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-18-CS.
           05  FILLER                  PIC  X(001).
           05  HX-NUM-18-SS            PIC  9(018).

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS SEM CASAS DECIMAIS PARA       *
      *    REDUZIR TAMANHO DO CAMPO.                                   *
      *----------------------------------------------------------------*

       01  HX-NRO-18                   PIC  9(018)        VALUE ZEROS.

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(001).
           05 HX-NRO-17                PIC  9(017).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(002).
           05 HX-NRO-16                PIC  9(016).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(003).
           05 HX-NRO-15                PIC  9(015).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(004).
           05 HX-NRO-14                PIC  9(014).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(005).
           05 HX-NRO-13                PIC  9(013).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(006).
           05 HX-NRO-12                PIC  9(012).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(007).
           05 HX-NRO-11                PIC  9(011).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(008).
           05 HX-NRO-10                PIC  9(010).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(009).
           05 HX-NRO-09                PIC  9(009).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(010).
           05 HX-NRO-08                PIC  9(008).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(011).
           05 HX-NRO-07                PIC  9(007).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(012).
           05 HX-NRO-06                PIC  9(006).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(013).
           05 HX-NRO-05                PIC  9(005).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(014).
           05 HX-NRO-04                PIC  9(004).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(015).
           05 HX-NRO-03                PIC  9(003).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(016).
           05 HX-NRO-02                PIC  9(002).

       01  FILLER REDEFINES            HX-NRO-18.
           05 FILLER                   PIC  X(017).
           05 HX-NRO-01                PIC  9(001).

      *----------------------------------------------------------------*
      *    AREA PARA TROCAR FORMATO DE VARIAVEIS                       *
      *----------------------------------------------------------------*

       01  HX-ALF-001.
           05 HX-NUM-001               PIC  9(001)         VALUE ZEROS.

       01  HX-ALF-002.
           05 HX-NUM-002               PIC  9(002)         VALUE ZEROS.

       01  HX-ALF-003.
           05 HX-NUM-003               PIC  9(003)         VALUE ZEROS.

       01  HX-ALF-004.
           05 HX-NUM-004               PIC  9(004)         VALUE ZEROS.

       01  HX-ALF-005.
           05 HX-NUM-005               PIC  9(005)         VALUE ZEROS.

       01  HX-ALF-006.
           05 HX-NUM-006               PIC  9(006)         VALUE ZEROS.

       01  HX-ALF-007.
           05 HX-NUM-007               PIC  9(007)         VALUE ZEROS.

       01  HX-ALF-008.
           05 HX-NUM-008               PIC  9(008)         VALUE ZEROS.

       01  HX-ALF-009.
           05 HX-NUM-009               PIC  9(009)         VALUE ZEROS.

       01  HX-ALF-010.
           05 HX-NUM-010               PIC  9(010)         VALUE ZEROS.

       01  HX-ALF-011.
           05 HX-NUM-011               PIC  9(011)         VALUE ZEROS.

       01  HX-ALF-012.
           05 HX-NUM-012               PIC  9(012)         VALUE ZEROS.

       01  HX-ALF-013.
           05 HX-NUM-013               PIC  9(013)         VALUE ZEROS.

       01  HX-ALF-014.
           05 HX-NUM-014               PIC  9(014)         VALUE ZEROS.

       01  HX-ALF-015.
           05 HX-NUM-015               PIC  9(015)         VALUE ZEROS.

       01  HX-ALF-016.
           05 HX-NUM-016               PIC  9(016)         VALUE ZEROS.

       01  HX-ALF-017.
           05 HX-NUM-017               PIC  9(017)         VALUE ZEROS.

       01  HX-ALF-018.
           05 HX-NUM-018               PIC  9(018)         VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS COM 2 CASAS DECIMAIS - MASCARA*
      *----------------------------------------------------------------*

       01  HX-NUM-01V02-CS             PIC +9(001)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-01V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-01V02-SS          PIC  9(001)V9(002).

       01  HX-NUM-02V02-CS             PIC +9(002)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-02V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-02V02-SS          PIC  9(002)V9(002).

       01  HX-NUM-03V02-CS             PIC +9(003)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-03V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-03V02-SS          PIC  9(003)V9(002).

       01  HX-NUM-04V02-CS             PIC +9(004)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-04V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-04V02-SS          PIC  9(004)V9(002).

       01  HX-NUM-05V02-CS             PIC +9(005)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-05V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-05V02-SS          PIC  9(005)V9(002).

       01  HX-NUM-06V02-CS             PIC +9(006)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-06V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-06V02-SS          PIC  9(006)V9(002).

       01  HX-NUM-07V02-CS             PIC +9(007)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-07V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-07V02-SS          PIC  9(007)V9(002).

       01  HX-NUM-08V02-CS             PIC +9(008)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-08V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-08V02-SS          PIC  9(008)V9(002).

       01  HX-NUM-09V02-CS             PIC +9(009)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-09V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-09V02-SS          PIC  9(009)V9(002).

       01  HX-NUM-10V02-CS             PIC +9(010)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-10V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-10V02-SS          PIC  9(010)V9(002).

       01  HX-NUM-11V02-CS             PIC +9(011)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-11V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-11V02-SS          PIC  9(011)V9(002).

       01  HX-NUM-12V02-CS             PIC +9(012)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-12V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-12V02-SS          PIC  9(012)V9(002).

       01  HX-NUM-13V02-CS             PIC +9(013)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-13V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-13V02-SS          PIC  9(013)V9(002).

       01  HX-NUM-14V02-CS             PIC +9(014)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-14V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-14V02-SS          PIC  9(014)V9(002).

       01  HX-NUM-15V02-CS             PIC +9(015)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-15V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-15V02-SS          PIC  9(015)V9(002).

       01  HX-NUM-16V02-CS             PIC +9(016)V9(002)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-16V02-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-16V02-SS          PIC  9(016)V9(002).

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS COM 2 CASAS DECIMAIS PARA     *
      *    REDUZIR TAMANHO DO CAMPO.                                   *
      *----------------------------------------------------------------*

       01  HX-NRO-15V02                PIC  9(015)V9(002) VALUE ZEROS.

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(001).
           05 HX-NRO-14V02             PIC  9(014)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(002).
           05 HX-NRO-13V02             PIC  9(013)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(003).
           05 HX-NRO-12V02             PIC  9(012)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(004).
           05 HX-NRO-11V02             PIC  9(011)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(005).
           05 HX-NRO-10V02             PIC  9(010)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(006).
           05 HX-NRO-09V02             PIC  9(009)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(007).
           05 HX-NRO-08V02             PIC  9(008)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(008).
           05 HX-NRO-07V02             PIC  9(007)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(009).
           05 HX-NRO-06V02             PIC  9(006)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(010).
           05 HX-NRO-05V02             PIC  9(005)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(011).
           05 HX-NRO-04V02             PIC S9(004)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(012).
           05 HX-NRO-03V02             PIC  9(003)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(013).
           05 HX-NRO-02V02             PIC  9(002)V9(002).

       01  FILLER REDEFINES            HX-NRO-15V02.
           05 FILLER                   PIC  X(014).
           05 HX-NRO-01V02             PIC  9(001)V9(002).

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS COM CASAS DECIMAIS.           *
      *----------------------------------------------------------------*

       01  HX-NUM-03V05-CS             PIC +9(003)V9(005)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-03V05-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-03V05-SS          PIC  9(003)V9(005).

       01  HX-NUM-03V06-CS             PIC +9(003)V9(006)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-03V06-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-03V06-SS          PIC  9(003)V9(006).

       01  HX-NUM-09V09-CS             PIC +9(009)V9(009)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-09V09-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-09V09-SS          PIC  9(009)V9(009).

       01  HX-NUM-09V06-CS             PIC +9(009)V9(006)  VALUE ZEROS.
       01  FILLER REDEFINES            HX-NUM-09V06-CS.
           05 FILLER                   PIC  X(001).
           05 HX-NUM-09V06-SS          PIC  9(009)V9(006).

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS COM CASAS DECIMAIS PARA REDU- *
      *    ZIR TAMANHO DE CAMPOS.                                      *
      *----------------------------------------------------------------*

       01  HX-NRO-09V09                PIC  9(009)V9(009)  VALUE ZEROS.

       01  FILLER REDEFINES            HX-NRO-09V09.
           05 FILLER                   PIC  X(006).
           05 HX-NUM-03V02             PIC  9(003)V9(002).
           05 FILLER                   PIC  X(007).

       01  FILLER REDEFINES            HX-NRO-09V09.
           05 FILLER                   PIC  X(006).
           05 HX-NRO-03V03             PIC  9(003)V9(003).
           05 FILLER                   PIC  X(006).

       01  FILLER REDEFINES            HX-NRO-09V09.
           05 FILLER                   PIC  X(006).
           05 HX-NRO-03V05             PIC  9(003)V9(005).
           05 FILLER                   PIC  X(004).

       01  FILLER REDEFINES            HX-NRO-09V09.
           05 FILLER                   PIC  X(006).
           05 HX-NRO-03V06             PIC  9(003)V9(006).
           05 FILLER                   PIC  X(003).

       01  FILLER REDEFINES            HX-NRO-09V09.
           05 FILLER                   PIC  X(007).
           05 HX-NRO-02V05             PIC  9(002)V9(005).
           05 FILLER                   PIC  X(004).

       01  FILLER REDEFINES            HX-NRO-09V09.
           05 FILLER                   PIC  X(007).
           05 HX-NRO-02V06             PIC  9(002)V9(006).
           05 FILLER                   PIC  X(003).

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS NUMERICAS PARA REDUZIR TAMANHO DE CAM-  *
      *    POS ESPECIFICOS.                                            *
      *----------------------------------------------------------------*

       01  HX-CONTA-13                 PIC +9(013)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CONTA-13.
           05 FILLER                   PIC  X(001).
           05 FILLER                   PIC  9(006).
           05 HX-CONTA-07              PIC  9(007).
       01  FILLER REDEFINES            HX-CONTA-13.
           05 FILLER                   PIC  X(001).
           05 FILLER                   PIC  9(002).
           05 HX-CONTA-11              PIC  9(011).

       01  HX-CNEGOC-COBR              PIC +9(018)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CNEGOC-COBR.
           05 FILLER                   PIC  X(008).
           05 HX-CNEGOC-AGE            PIC  9(004).
           05 HX-CNEGOC-CTA            PIC  9(007).

       01  HX-CTITLO-COBR-BCO          PIC +9(018)         VALUE ZEROS.
       01  FILLER                      REDEFINES HX-CTITLO-COBR-BCO.
           05 FILLER                   PIC  X(008).
           05 HX-CTITLO-COBR-BCO-02    PIC  9(002).
           05 HX-CTITLO-COBR-BCO-09    PIC  9(009).
       01  HX-CTITLO-COBR-BCO-18       REDEFINES HX-CTITLO-COBR-BCO.
           05 FILLER                   PIC  X(001).
           05 FILLER                   PIC  9(007).
           05 HX-CTITLO-COBR-BCO-11    PIC  9(011).

       01  HX-CPF-CNPJ                 PIC  9(015)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CPF-CNPJ.
           05 HX-CCNPJ-CPF             PIC  9(009).
           05 HX-CFLIAL-CNPJ           PIC  9(004).
           05 HX-CCTRL-CNPJ-CPF        PIC  9(002).

       01  HX-CONSULTA-CONE            PIC  9(018)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CONSULTA-CONE.
           05 HX-DANO-BASE             PIC  9(004).
           05 HX-CDEPDC                PIC  9(005).
           05 HX-CSEQ-STUDO            PIC  9(009).

       01  HX-COD-DEPTO-06.
           05 FILLER                   PIC  X(002)         VALUE SPACES.
           05 HX-COD-DEPTO-04          PIC  9(004)         VALUE ZEROS.

       01  HX-CFUNC-BDSCO-09           PIC +9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CFUNC-BDSCO-09.
           05 FILLER                   PIC  X(001).
           05 FILLER                   PIC  9(002).
           05 HX-CFUNC-BDSCO-07        PIC  9(007).

       01  HX-SQLCODE-09               PIC +9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-SQLCODE-09.
           05 HX-SQLCODE-SINAL         PIC  X(001).
           05 FILLER                   PIC  9(005).
           05 HX-SQLCODE-04            PIC  9(004).

       01  HX-COD-RETORNO.
           05 HX-COD-RETORNO-N         PIC  9(004)         VALUE ZEROS.
           05 FILLER REDEFINES         HX-COD-RETORNO-N.
              10 FILLER                PIC  9(002).
              10 HX-COD-RETORNO-02     PIC  9(002).

       01  HX-OPERACAO                 PIC +9(013)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-OPERACAO.
           05 FILLER                   PIC  X(001).
           05 HX-DANO-OPER-DESC        PIC  9(004).
           05 HX-NSEQ-OPER-DESC        PIC  9(009).

       01  HX-CCCORR-CHEQC-13          PIC +9(013)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CCCORR-CHEQC-13.
           05 FILLER                   PIC  X(001).
           05 HX-FILLER                PIC  9(002).
           05 HX-CCCORR-CHEQC-11       PIC  9(011).

       01  HX-DATA-09                  PIC  9(009)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-DATA-09.
           05 FILLER                   PIC  9(001).
           05 HX-DATA-08               PIC  9(008).

       01  HX-CCEP                     PIC  9(008)         VALUE ZEROS.
       01  FILLER REDEFINES            HX-CCEP.
           05 HX-COD-CEP               PIC  9(005).
           05 HX-COMPL-CEP             PIC  9(003).

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS AUXILIARES COMPACTADAS                  *
      *----------------------------------------------------------------*

       01  HX-QTDE-RETORNADA           PIC  9(003) COMP-3  VALUE ZEROS.
       01  HX-PROXIMO-RESTART          PIC  9(005) COMP-3  VALUE ZEROS.
       01  HX-QTDE-A-DESPREZAR         PIC  9(005) COMP-3  VALUE ZEROS.

       01  HX-PAGINA                   PIC  9(004) COMP-3  VALUE ZEROS.
       01  HX-RESTART                  PIC  9(005) COMP-3  VALUE ZEROS.

       01  HX-VLR-15V02                PIC  9(015)V9(002)
                                                   COMP-3  VALUE ZEROS.
       01  HX-VLR-13V02                PIC  9(013)V9(002)
                                                   COMP-3  VALUE ZEROS.
       01  HX-VLR-09V02                PIC  9(009)V9(002)
                                                   COMP-3  VALUE ZEROS.

       01  HX-NUM-03-COMP              PIC  9(003) COMP-3  VALUE ZEROS.
       01  HX-NUM-05-COMP              PIC  9(005) COMP-3  VALUE ZEROS.
       01  HX-NUM-07-COMP              PIC  9(007) COMP-3  VALUE ZEROS.
       01  HX-NUM-09-COMP              PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
      *    AREA PARA VARIAVEIS DE DATA                                 *
      *----------------------------------------------------------------*

       01  HX-DATA-DB2.
           05  HX-DIA-DB2              PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  HX-MES-DB2              PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  HX-ANO-DB2              PIC  9(004)         VALUE ZEROS.

       01  HX-DATA-INV                 PIC  9(008)         VALUE ZEROS.
       01  HX-DATA-INV-R               REDEFINES           HX-DATA-INV.
           05  HX-ANO-INV              PIC  9(004).
           05  HX-MES-INV              PIC  9(002).
           05  HX-DIA-INV              PIC  9(002).

