SET DATEFIRST 7
SET ANSI_NULLS OFF
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
SET LOCK_TIMEOUT -1
SET QUOTED_IDENTIFIER OFF
GO

IF EXISTS(SELECT * FROM sysobjects WHERE TYPE='p' AND NAME='spCEGenerarVenta')
DROP PROCEDURE spCEGenerarVenta
GO
CREATE PROCEDURE  spCEGenerarVenta
  @Empresa varchar(5),
  @Sucursal int,
  @Accion varchar(20),
  @Usuario varchar(10),
  @Alumno varchar(20),
  @Mov varchar(20),
  @MovTipo varchar(20),
  @ID int,              
  @Ok int OUTPUT,
  @OkRef varchar(255) OUTPUT,
  @CalcularVenta varchar(20)            

AS BEGIN
  DECLARE
	@Tipo varchar(50),
	@Moneda varchar(10),
	@TipoCambio float,
	@IDCuota int,
	@FechaEmision datetime,
	@MovID varchar(20),
	@IDGenerar int,
	@MovGenerar varchar(20),
	@MovGenerarD varchar(20),
	@MovIDGenerar varchar(20),      
	@Articulo varchar(20),
	@SubCuenta varchar(50),
	@Cantidad float,
	@Unidad varchar(50),
	@ArtTipo varchar(20),    
	@Precio float,
	@Renglon float,
	@RenglonID int,
	@RenglonTipo char(1),
	@Concepto varchar(20),
	@Almacen varchar(20),
	@CicloEscolar varchar(20),
	@Programa varchar(10),
	@PlanEstudios varchar(10),
	@Cuota float, 
	@FacturaMultiple bit,
	@Cliente varchar(10),
	@CantCreditos float,
	@CantMaterias int,
	@TipoCuota varchar(50),
	@ClienteD varchar(10),
	@PorcCliente float,
	@CubreTodo bit,
	@CubreAdmision bit,
	@CubreInscripcion bit,
	@CubreColegiatura bit,
	@CubreCuotasAdicionales bit,
	@CubreServicioSocial bit,
	@CubreTitulacion bit,
	@CubreExtraordinario bit,
	@CubreOtros bit,
	@FacturarA varchar(10),
	@ArticuloBeca varchar(20),
	@Rubro varchar(50),
	@PorcBeca float,
	@ImporteV float,
	@CreditosV float,
	@MateriasV int,
	@ZonaImpuesto varchar(50),
	@Impuesto1 float,
	@Impuesto2 float,
	@Impuesto3 money,
	@ClienteV varchar(10),
	@CondicionPago varchar(50),
	@ConceptoVenta varchar(50),
	@ConceptoVentaCte varchar(50),
	@Beca varchar(50),
	@BecaB varchar(50),
	@ArticuloV varchar(20),
	@ArticuloB varchar(20),
	@PorcentajeV float,
	@Importe float,
	@PrecioB float,
	@IDGenerarBeca int,
	@DiaV int,
	@Dia int,
	@Mes int,
	@MesV int,
	@Materia varchar(20),
	@ArticuloMov varchar(20),
	@ListaPrecios varchar(20),
	@PrecioMateria float,
	@ConceptoBeca varchar(50),
	@MovBeca varchar(20),
	@NombreCuota varchar(50),
	@IDTemp int,
	@CantidadV float,
	@Fecha datetime,
	@Fecha2 datetime,
	@PorcRecargo float,
	@PorcRecargoB float,
	@ArtRecargo varchar(20),
	@ArtRecargoB varchar(20),
	@Frecuencia varchar(20),
	@FrecuenciaB varchar(20),
	@EstatusD varchar(15),
	@NumPagos int,
	@FechaCiclo datetime,
	@ConceptoCuota varchar(50),
	@ID2D int,
	@RID2 int,
	@DiaB int,
	@MesD int,
	@ImporteBeca float,
	@NumPago int,
	@Pagos int,
	@PagoNumV int,
	@PagosV int,
	@BecaV varchar(50),
	@PagoNumB int,
	@PagosB int,   
	@ConceptoCuotaB varchar(50),
	@ConceptoCuotaBD varchar(50),
	@ConceptoCuotaVD varchar(50),
	@Observaciones varchar(100),
	@DiaO int,
	@MesO int,
	@CentroCosto varchar(20),
	@RecSrec bit,
	@RecSrecB bit,
	@RecSrec2 bit,
	@ContMoneda varchar(10),
	@BecasCascada bit,
	@AlumnoRefVTAS varchar(50),
    @AlumnoComVTAS varchar(50),
	@RecargoFijo float, 
	@RecargoTope float,
	@RecargoFijoB float,
	@RecargoTopeB float,
	@BanderaRecargoFijo bit,
	@ConceptoRecargo varchar(50),  
	@MovRecargoPlan varchar(20),
	@MovRecargo varchar(20),
	@ConceptoRecargoB varchar(50),  
	@MovRecargoB varchar(20),
	@ArticuloParaAplicarBeca varchar(20),
	@CausaVenta varchar(50),
	@CausaRecargo varchar(50),
	@NombreCuotaIE varchar(50),
	@SucursalAlumno int,
	@GenerarRefVTAS bit,
	@DescuentoLinea money,
	@UEN int,
	@Comentarios varchar(MAX),
	@NumDocumento varchar(20),
	@NoGenerarRecargos bit,
	 @alumnofbh varchar(10)

  DECLARE @Tabla table (
	ID int identity(1,1),
	RID int,
	Tipo varchar(20),
	Importe float   ,
	ConceptoCuota varchar(50),
	Rubro varchar(50)
  )

  DECLARE @Tabla2 table (
    ID int identity(1,1),
    RID int,
    Cliente varchar(10),
    CondicionPago varchar(50),
    ConceptoVenta varchar(50),
    NombreCuota varchar(50),
    CuotaConcepto varchar(50),
    MovVenta varchar(20),
    Rubro varchar(50)
  )

  DECLARE @Tabla2D table(
    ID int,
    RID int,
    Alumno varchar(20),
    Articulo varchar(20),
    Importe float,
	DescuentoLinea money,
    Porcentaje float,
    Creditos float,
    Materias int,
    PorcCliente float,
    Cantidad int,
    Dia int,
    Mes int,
    PorcRecargo float, 
    RecargoFijo float,
    RecargoTope float,
    ArtRecargo varchar(20),
    Frecuencia varchar(20),
    PagoNum int,
    Pagos int,
    Beca varchar(50),
    EsBeca bit,
    ConceptoCuota varchar(50),
    Observaciones varchar(100),
    RecSrec bit,
    ConceptoRecargo varchar(50),  
    MovRecargo varchar(20),
	Cliente varchar(10),
	CausaVenta varchar(50),
	CausaRecargo varchar(50)
  )

  DECLARE @TablaBeca table (
	Beca varchar(50),
    Cliente varchar(10),  
    Articulo varchar(20),
    Importe float,
    Moneda varchar(10),
    TipoCambio float,
    Unidad varchar(50),
    PagoNum int,
    Pagos int,
    ConceptoCuota varchar(50),
	Causa varchar(50)
  )
  
  IF (@CalcularVenta = 'Cuotas Kardex')  
  BEGIN  
    DECLARE @CuotasKardex as table (
	  ID int identity,
	  Empresa varchar(5) null,
	  Mov varchar(20) null,
	  FechaEmision datetime null,
	  Concepto varchar(50) null,
	  Moneda varchar(10) null,
	  TipoCambio float null,
	  Usuario varchar(10) null,
	  Condicion varchar(50) null,
	  Estatus varchar(15) null,
	  Directo bit null,
	  Prioridad varchar(10) null,
	  Cliente varchar(10) null,
	  Almacen varchar(10) null,
	  FechaRequerida datetime null,
	  Vencimiento datetime null,
	  Sucursal int null,
	  OrigenTipo varchar(10) null,
	  Origen varchar(20) null,
	  OrigenID varchar(20) null,
	  DocFuente int null,
	  ContUso varchar(20) null,
	  Referencia varchar(50) null,
	  PorcRecargo float null,
	  ArtRecargo varchar(20) null,
	  Frecuencia varchar(20) null
	)
    
	DECLARE @CuotasKardexD as table (
	  ID int null,
	  Renglon float null,
	  RenglonSub int null,
	  RenglonID int null,
	  RenglonTipo varchar(1) null,
	  Cantidad float null,
	  CantidadPendiente float null,
	  Unidad varchar(50) null,
	  Almacen varchar(10) null,
	  Articulo varchar(20) null,
	  SubCuenta varchar(50) null,
	  Precio float null,
	  FechaRequerida datetime null,
	  Sucursal int null,
	  CantidadInventario float null,
	  Alumno varchar(20) null,
	  FechaVencimiento datetime null,
	  PorcRecargo float null,
	  ArtRecargo varchar(20) null,
	  Frecuencia varchar(20) null,
	  MovGenerar varchar(20) null,
	  EsBeca bit null,
	  Beca varchar(50) null,
	  EstatusD varchar(15) null,
	  Impuesto1 float null,
	  Impuesto2 float null,
	  Impuesto3 float null,
	  ConceptoCuota varchar(50) null,
	  NumDocumento varchar(20) null,
	  DescripcionExtra varchar(100) null,
	  ContUso varchar (20) null,
	  RecSRec bit null
	)
  END
  
  SET DATEFORMAT DMY

  SELECT @FechaEmision = dbo.fnFechaSinHora(GETDATE()),
         @DiaO         = DATEPART(DAY,@FechaEmision),
         @MesO         = DATEPART(MONTH,@FechaEmision)
  
  SELECT @BecasCascada   = BecasCascada, 
	     @GenerarRefVTAS = AlumnoRefVTAS,
		 @Almacen        = AlmacenServicio
	FROM EmpresaCfgCE
   WHERE Empresa = @Empresa

  -- Seleccionar el almacen de la sucursal si esta configurado
  IF EXISTS (SELECT AlmacenPrincipal FROM Sucursal WHERE Sucursal = @Sucursal)
    SELECT @Almacen = AlmacenPrincipal FROM Sucursal WHERE Sucursal = @Sucursal

  IF ISNULL(@Almacen,'') = ''
    SELECT @Ok = 1000215 -- Falta configurar el Almacen de Servicios en Empresa General - Educación

  SELECT @ContMoneda = ContMoneda FROM EmpresaCfg WHERE Empresa = @Empresa

  SELECT @MovID             = CE.MovID,
		 @PlanEstudios      = CE.PlanEstudios,
		 @Programa          = CE.Programa,
		 @CicloEscolar      = CE.CicloEscolar,
		 @CentroCosto       = CE.CentroCosto,
		 @CantCreditos      = ISNULL(CE.NumCreditos,0),
	     @CantMaterias      = ISNULL(CE.NumMaterias,0),
		 @NombreCuotaIE     = CE.NombreCuotaIE,
		 @AlumnoRefVTAS     = CEAlumno.Alumno + ': ' + CEAlumno.Nombre + ' ' + CEAlumno.ApellidoPaterno + ' ' + CEAlumno.ApellidoMaterno,
		 @Cliente           = CEAlumno.FacturarCte,
		 @alumnofbh         =ce.alumno,
	     @FacturaMultiple   = CEAlumno.FacturaMultiple,
		 @SucursalAlumno    = CEAlumno.Sucursal,
		 @ListaPrecios      = CEAlumno.ListaPrecioEsp,
		 @NoGenerarRecargos = ISNULL(CEAlumno.NoGenerarCuotasJob,0),
		 @Comentarios       = '********** Control Escolar **********' + CHAR(13)
		                     + CHAR(13)
		                     + 'Movimiento: ' + ISNULL(CE.Mov,'') + '[' + ISNULL(CE.MovID,'') + ']' + CHAR(13) 
							 + 'Alumno: '            + ISNULL(CEAlumno.Alumno,'') + CHAR(13) 
							 + 'Matricula:'          + ISNULL(CE.Matricula,'') + CHAR(13)
							 + 'Nombre del Alumno: ' + ISNULL(CEAlumno.Nombre,'') + ' ' + ISNULL(CEAlumno.ApellidoPaterno,'') + ' ' + ISNULL(CEAlumno.ApellidoMaterno,'') + CHAR(13) 
							 + 'CURP: '              + ISNULL(CEAlumno.CURP,'') + CHAR(13) 
							 + 'Ciclo Escolar: '     + ISNULL(CE.CicloEscolar,'') + CHAR(13)
							 + 'Nivel Académico: '   + ISNULL(CE.NivelAcademico,'') + CHAR(13) 
							 + 'Programa: '          + ISNULL(CE.Programa,'') + CHAR(13) 
							 + 'Plan de Estudio: '   + ISNULL(CE.PlanEstudios,'') + CHAR(13)
    FROM CE
	JOIN CEAlumno ON CE.Alumno = CEAlumno.Alumno
   WHERE ID = @ID 

  IF (@GenerarRefVTAS <> 1)
	SET @AlumnoRefVTAS = null

  -- LAGT Cobros Referenciados
  SELECT @AlumnoComVTAS = @AlumnoRefVTAS, @AlumnoRefVTAS = ReferenciaBancaria FROM Cte WHERE Cliente = @alumnofbh

  SELECT @FechaCiclo = InicioCiclo
    FROM CECicloEscolar
   WHERE Empresa = @Empresa
	 AND CicloEscolar = @CicloEscolar
    
  SELECT @MovRecargoPlan = MovRecargo,
	     @BanderaRecargoFijo = isnull(RecargoFijo,0)
    FROM CEPlanEstudios
   WHERE Empresa = @Empresa
	 AND PlanEstudios = @PlanEstudios
	 AND Programa = @Programa

  IF @MovTipo IN('CE.PRM')   
  BEGIN
    SELECT @CantMaterias = ISNULL(COUNT(Materia),0)
      FROM CED 
     WHERE ID = @ID
	   AND ISNULL(EstatusD,'PENDIENTE') = 'PENDIENTE'
     
    SELECT @CantCreditos = ISNULL(SUM(p.Creditos),0)
      FROM CE c
      JOIN CED d ON c.ID = d.ID
      JOIN CEPLanEstudiosMaterias p ON d.Materia = p.Materia
	   AND c.Empresa = p.Empresa 
	   AND p.PlanEstudios = c.PlanEstudios
   	   AND c.Programa= p.Programa
	   AND ISNULL(c.SubPrograma,'') = ISNULL(p.SubPrograma,'')
     WHERE c.ID = @ID
	   AND ISNULL(EstatusD,'PENDIENTE') = 'PENDIENTE'
  END

  IF @MovTipo NOT IN('CE.NI','CE.I','CE.PRM')   
    SELECT @CantCreditos = SUM(Creditos),
	       @CantMaterias = SUM(Materias)
      FROM CECantidadMateriasCursando 
     WHERE Alumno = @Alumno
	   AND Empresa = @Empresa
	   AND PlanEstudios = @PlanEstudios
	   AND Programa = @Programa
	   AND CicloEscolar = @CicloEscolar

  -- RG Si existen cuotas configuradas con el Plantel del Alumno se toman estas
  IF @SucursalAlumno IN (SELECT Sucursal FROM CEPlanEstudiosCuotasMovimientos WHERE Empresa = @Empresa AND PlanEstudios = @PlanEstudios AND Programa = @Programa AND CicloEscolar = @CicloEscolar AND MovCE = @Mov GROUP BY Sucursal)
  BEGIN
    INSERT @Tabla(RID, Importe, Tipo, ConceptoCuota, Rubro)
    SELECT ID, Cuota, Tipo, MovCE, Rubro
      FROM CEPlanEstudiosCuotasMovimientos
     WHERE Empresa = @Empresa AND PlanEstudios = @PlanEstudios AND Programa = @Programa AND CicloEscolar = @CicloEscolar AND MovCE = @Mov AND Sucursal = @SucursalAlumno
     GROUP BY ID, Cuota, Tipo, MovCE, Rubro
      
		UNION
      
		SELECT ID, Cuota, Tipo, MovCE, Rubro
      FROM CEPlanEstudiosCuotasMovimientos
     WHERE Empresa = @Empresa AND PlanEstudios = @PlanEstudios AND Programa = @Programa AND CicloEscolar = @CicloEscolar AND MovCE = @Mov AND isnull(Sucursal,'') = ''
     GROUP BY ID, Cuota, Tipo, MovCE, Rubro 
    
	-- RG Si no existen cuotas configuradas con el Plantel del Alumno se toman las cuotas que no tienen Plantel configurado
  
  END ELSE BEGIN
	
	INSERT @Tabla(RID, Importe, Tipo, ConceptoCuota, Rubro)
    SELECT ID, Cuota, Tipo, MovCE, Rubro
      FROM CEPlanEstudiosCuotasMovimientos 
     WHERE Empresa = @Empresa AND PlanEstudios = @PlanEstudios AND Programa = @Programa AND CicloEscolar = @CicloEscolar AND MovCE = @Mov AND isnull(Sucursal,'') = ''
     GROUP BY ID, Cuota, Tipo, MovCE, Rubro 
  END

  --RG Se eliminan las cuotas de vigencia diferentes a la seleccionada en el movimiento 
  IF EXISTS (SELECT * FROM @Tabla WHERE Tipo = 'Vigencia')
    DELETE FROM @Tabla
	 WHERE Tipo = 'Vigencia'
	   AND RID <> (SELECT ID FROM CEPlanEstudiosCuotasMovimientos
                    WHERE Empresa = @Empresa AND PlanEstudios = @PlanEstudios AND Programa = @Programa
					   AND CicloEscolar = @CicloEscolar AND MovCE = @Mov AND NombreCuota = @NombreCuotaIE)         
  
  -- Falta configurar las Cuotas Movimientos en el Plan de Estudios
  IF NOT EXISTS (SELECT * FROM @Tabla)
    SELECT @Ok = 1000216

IF @OK IS NULL
BEGIN   

  DECLARE crCuotasMov CURSOR FOR   
  SELECT RID, Tipo, Importe, ConceptoCuota, Rubro
    FROM @Tabla 
   
  OPEN crCuotasMov
  FETCH NEXT FROM crCuotasMov INTO @IDCuota, @Tipo, @Cuota, @ConceptoCuota, @Rubro          
  WHILE (@@FETCH_STATUS = 0) AND (@Ok IS NULL)
  BEGIN
    
	SELECT @MovGenerar = MovVenta
	  FROM MovTipo
	 WHERE Mov = @Mov
	   AND Modulo = 'CE'
          
    IF (@FacturaMultiple = 0)
    BEGIN

      SET @TipoCuota = NULL
      
      IF ISNULL(@Cliente,'') = ''
	    SELECT @Ok = 55115, @OkRef = @OkRef +' Alumno: ' + @Alumno

      SELECT @TipoCuota        = NombreCuota,
			 @ConceptoVentaCte = ConceptoVenta
        FROM CECuotasClientes
       WHERE ConceptoCuota = @ConceptoCuota
	     AND Cliente       = @Cliente
		 AND Alumno        =@Alumno
           
      IF @TipoCuota IS NOT NULL AND NOT EXISTS (SELECT * FROM CEPECuotasMovNombreCuota WHERE ID = @IDCuota AND NombreCuota = @TipoCuota)
        SET @TipoCuota = NULL              
            
      IF (@TipoCuota IS NULL)
	  BEGIN
        SELECT @TipoCuota = NombreCuota
          FROM CEPECuotasMovNombreCuota
         WHERE ID = @IDCuota 
		   AND PorOmision = 1    
	  END
            
      SELECT @NumPagos = ISNULL(SUM(Cantidad),1)
        FROM CETiposCuotasD
       WHERE Cuota = @TipoCuota
		 AND Empresa = @Empresa
           
      SET @Pagos = NULL

	  INSERT @Tabla2(RID, Cliente, CondicionPago, ConceptoVenta, NombreCuota, CuotaConcepto, MovVenta, Rubro)
      SELECT @IDCuota, @Cliente, CondicionPago, ISNULL(@ConceptoVentaCte,ConceptoVenta), NombreCuota, @ConceptoCuota, @MovGenerar, @Rubro
        FROM CEPECuotasMovNombreCuota 
       WHERE ID = @IDCuota
	     AND NombreCuota = @TipoCuota            
      
	  SELECT @IDTemp = SCOPE_IDENTITY()    
      
	  IF @@ERROR <> 0 SELECT @Ok = 1

	  IF @Ok IS NULL
	  IF @Tipo IN ('Fijo','Vigencia') --Para Facturar a un solo Cliente
      BEGIN
   
	    -- Modificado por PE 14/06/2016; El día en fechas inicia en cero, debemos quitar un dia a lo configurado en tipo de cuotas
        INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
        SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, 100, ISNULL(t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t
.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota,ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @Cliente, CausaVenta, CausaRecargo
          FROM CEPECuotasMovNombreCuota c
          LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
         WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
      
	  END ELSE IF @Tipo = 'Por Credito' --Para Facturar a un solo Cliente
      BEGIN
      
        INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
        SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), ISNULL(@CantCreditos,1), 1, 100, ISNULL(t.Cantidad,1)*ISNULL(@CantCreditos,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @Cliente, CausaVenta, CausaRecargo
          FROM CEPECuotasMovNombreCuota c
          LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa 
         WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
      
	  END ELSE IF @Tipo = 'Por Credito Rango' --Para Facturar a un solo Cliente
      BEGIN

        SELECT @Cuota = Cuota FROM CEPECuotasMovRangos WHERE ID = @IDCuota AND ISNULL(@CantCreditos,1) BETWEEN De AND Hasta
          
        INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, Observaciones, RecSrec, ConceptoRecargo, MovRecargo,
 Cliente, CausaVenta, CausaRecargo)
        SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, 100, ISNULL(t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t
.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END,CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, CONVERT(varchar,ISNULL(@CantCreditos,1))+' 
Creditos', ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @Cliente, CausaVenta, CausaRecargo
          FROM CEPECuotasMovNombreCuota c
          LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
         WHERE ID = @IDCuota AND NombreCuota = @TipoCuota  
      
	  END ELSE IF @Tipo = 'Por Materia' --Para Facturar a un solo Cliente
      BEGIN
      
        IF @MovTipo IN ('CE.PRM','CE.RM')   
        BEGIN
          DECLARE crCursorMateria CURSOR FOR   
          SELECT Materia
            FROM CED
           WHERE ID = @ID  
         
          OPEN crCursorMateria
          FETCH NEXT FROM crCursorMateria INTO @Materia
          WHILE (@@FETCH_STATUS = 0)
          BEGIN
            SELECT @ArticuloMov = Articulo FROM CEMateria WHERE Materia = @Materia AND Empresa = @Empresa
            IF isnull(@ArticuloMov,'') = ''
            SELECT @ArticuloMov =  Articulo FROM CEPECuotasMovNombreCuota WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
            SELECT @Unidad = Unidad FROM Art WHERE Articulo = @ArticuloMov
            SELECT @Moneda = ISNULL(NULLIF(DefMoneda,''),@ContMoneda) FROM Cte WHERE Cliente= @Cliente
            SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda
            EXEC spPCGet @Sucursal, @Empresa, @ArticuloMov, @SubCuenta, @Unidad, @Moneda, @TipoCambio, @ListaPrecios, @PrecioMateria OUTPUT
             
            INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia,  Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, 
CausaVenta, CausaRecargo)
            SELECT @IDTemp, @IDCuota, @Alumno, ISNULL(@ArticuloMov,c.Articulo), (@PrecioMateria/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, 100, ISNULL(t.Cantidad,
1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, ISNULL(c.
RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @Cliente, CausaVenta, CausaRecargo
              FROM CEPECuotasMovNombreCuota c
              LEFT JOIN CETiposCuotasD t ON  t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
             WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
            IF @@ERROR <> 0 SELECT @Ok = 1   
                          
            FETCH NEXT FROM crCursorMateria INTO @Materia
          END
          CLOSE crCursorMateria
          DEALLOCATE crCursorMateria
           
		END ELSE IF @MovTipo NOT IN ('CE.PRM','CE.RM')
		BEGIN
		  SELECT @Moneda = ISNULL(NULLIF(DefMoneda,''),@ContMoneda) FROM Cte WHERE Cliente = @Cliente
          SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda
          
          INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
          SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (ISNULL(dbo.fnPCGet (@Empresa, @Sucursal, @Moneda,@TipoCambio, c.Articulo, NULL, NULL, @ListaPrecios),0.0)/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, ISNULL(@CantMaterias,1), 100, ISNULL(t.Cantidad,1)*ISNULL(@CantMaterias,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @Cliente, CausaVenta, CausaRecargo
            FROM CEPECuotasMovNombreCuota c
            LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
           WHERE ID = @IDCuota AND NombreCuota = @TipoCuota  
        END
           
	  END ELSE IF @Tipo = 'Por Materias Rango' --Para Facturar a un solo Cliente
      BEGIN

        SELECT @Cuota = Cuota FROM CEPECuotasMovRangos WHERE ID = @IDCuota AND ISNULL(@CantMaterias,1) BETWEEN De AND Hasta
        
        INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, Observaciones, RecSrec, ConceptoRecargo, MovRecargo,
 Cliente, CausaVenta, CausaRecargo)
        SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, 100, ISNULL(t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t
.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, CONVERT(varchar,ISNULL(@CantMaterias,1))+'
 Materias', ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @Cliente, CausaVenta, CausaRecargo
          FROM CEPECuotasMovNombreCuota c
          LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
         WHERE ID = @IDCuota AND NombreCuota = @TipoCuota                   
      END
      
	  IF @Ok IS NULL AND EXISTS (select * from @Tabla2D)
	  BEGIN
	    SELECT @Pagos = COUNT(*) FROM @Tabla2D WHERE ID = @IDTemp                 

        SET @NumPago = 0

        UPDATE @Tabla2D
           SET @Numpago = PagoNum = @Numpago + 1,
		       Pagos = @Pagos
          FROM @Tabla2D 
         WHERE ID = @IDTemp 
	  END
			
	  -- Becas para Facturar a un solo Cliente  
      IF @Ok IS NULL AND @Rubro IS NOT NULL
	  AND EXISTS (SELECT * FROM CEBecasActivas WHERE Empresa = @Empresa AND CicloEscolar = @CicloEscolar AND Programa = @Programa AND Alumno = @Alumno)
      BEGIN

        -- Modificado por RG 30/03/2015; Seleccionar solo las becas de la vista Becas Activas
        DECLARE crBecas CURSOR FOR
        SELECT Beca
		  FROM CEBecasActivas  
         WHERE Empresa = @Empresa
		   AND Alumno = @Alumno
		   AND CicloEscolar = @CicloEscolar
		   AND Programa = @Programa
    
        OPEN crBecas
        FETCH NEXT FROM crBecas INTO @Beca
        WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
        BEGIN 
          SELECT @ArticuloBeca = Articulo,
			     @PorcBeca = PorcdeDesc,
				 @CubreTodo = CubreTodo,
				 @CubreAdmision = CubreAdmision,
				 @CubreInscripcion = CubreInscripcion,
				 @CubreColegiatura = CubreColegiatura,
				 @CubreCuotasAdicionales = CubreCuotasAdicionales,
				 @CubreServicioSocial = CubreServicioSocial,
				 @CubreTitulacion = CubreTitulacion,
				 @CubreExtraordinario = CubreExtraordinario,
				 @CubreOtros = CubreOtros  
            FROM CEBecaCiclo
           WHERE Empresa = @Empresa
		     AND Ciclo = @CicloEscolar
			 AND Beca = @Beca 
         
          IF @CubreTodo = 1
			OR @Rubro = 'Admisión'               AND @CubreAdmision = 1
            OR @Rubro = 'Inscripción'            AND @CubreInscripcion = 1
            OR @Rubro = 'Colegiatura'            AND @CubreColegiatura = 1
            OR @Rubro = 'Cuotas Adicionales'     AND @CubreCuotasAdicionales = 1
            OR @Rubro = 'Servicio Social'        AND @CubreServicioSocial = 1
            OR @Rubro = 'Titulación'             AND @CubreTitulacion = 1
            OR @Rubro = 'Examen Extraordinarios' AND @CubreExtraordinario = 1
            OR @Rubro = 'Otros'                  AND @CubreOtros = 1
          BEGIN
              
            DECLARE crVentaBeca CURSOR FOR                    
            SELECT ID,
				   RID,
				   Articulo,
				   PorcCliente * (Importe * Cantidad + (Porcentaje * (Importe * Cantidad) / 100)) / 100,
				   Dia,
				   Mes,
				   PagoNum,
				   Pagos,
				   ConceptoCuota,
				   PorcRecargo,
				   RecargoFijo,
				   RecargoTope,
				   ArtRecargo,
				   Frecuencia,
				   RecSRec,
				   ConceptoRecargo,
				   MovRecargo,
				   Cliente,
				   CausaVenta
              FROM @Tabla2D 
             WHERE ID = @IDTemp
			   AND isnull(Importe,0) > 0
			   AND isnull(EsBeca,0) = 0
			   AND Articulo NOT IN (SELECT Articulo FROM Art WHERE CENoAplicaBeca = 1)
              
            OPEN crVentaBeca
            FETCH NEXT FROM crVentaBeca INTO @ID2D, @RID2, @ArticuloParaAplicarBeca, @ImporteBeca, @DiaB, @MesD, @PagoNumB, @PagosB, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente, @CausaVenta
            WHILE @@FETCH_STATUS = 0
            BEGIN 

              IF @BecasCascada = 1
			  BEGIN
				IF (@CalcularVenta <> 'Cuotas Kardex')
				BEGIN
				  UPDATE @Tabla2D
					 SET DescuentoLinea = ISNULL(DescuentoLinea,1) * 
						 CASE 
						   WHEN @PorcBeca = 100
						   THEN 100
						   ELSE (1-( cast(@PorcBeca as float) /100))
						 END,
						 PorcCliente = 100,
						 Beca = @Beca
				   WHERE ID = @ID2D
					 AND RID = @RID2
					 AND Articulo = @ArticuloParaAplicarBeca
					 AND Cliente = @Cliente
										  
				END ELSE BEGIN
				  IF (((ISNULL(@ImporteBeca + (SELECT SUM(Importe) FROM @Tabla2D WHERE Importe < 0),0.0) * @PorcBeca) / 100) * -1) = 0			
				    INSERT @Tabla2D (ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PagoNum, Pagos, EsBeca, Beca, ConceptoCuota, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, RecSRec, ConceptoRecargo, MovRecargo, Cliente)
				    SELECT @ID2D, @RID2, @Alumno, @ArticuloBeca, (((ISNULL(@ImporteBeca,0.0)*@PorcBeca)/100) * -1), 0, 1, 1, 100, 1, @DiaB, @MesD, @PagoNumB, @PagosB, 1, @Beca, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, 
@RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente
				  
				  ELSE
				    INSERT @Tabla2D (ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PagoNum, Pagos, EsBeca, Beca, ConceptoCuota, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, RecSRec, ConceptoRecargo, MovRecargo, Cliente)
					SELECT @ID2D, @RID2, @Alumno, @ArticuloBeca,(((ISNULL(@ImporteBeca + (SELECT SUM(Importe) FROM @Tabla2D WHERE Importe < 0),0.0) * @PorcBeca) / 100) * -1), 0, 1, 1, 100, 1, @DiaB, @MesD, @PagoNumB, @PagosB, 1, @Beca, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente
                END
              END ELSE BEGIN
				IF (@CalcularVenta <> 'Cuotas Kardex')
				  UPDATE @Tabla2D
			         SET DescuentoLinea = isnull(DescuentoLinea,0) + @PorcBeca,
						 PorcCliente    = 100,
						 Beca           = @Beca
				   WHERE ID       = @ID2D
					 AND RID      = @RID2
					 AND Articulo = @ArticuloParaAplicarBeca
					 AND Dia      = @DiaB
					 AND mes      = @MesD
					 AND PagoNum  = @PagoNumB
					 AND Cliente  = @Cliente                
				ELSE
                  INSERT @Tabla2D (ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PagoNum, Pagos, EsBeca, Beca, ConceptoCuota, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, RecSRec, ConceptoRecargo, MovRecargo, Cliente)
                  SELECT @ID2D, @RID2, @Alumno, @ArticuloBeca, (((ISNULL(@ImporteBeca,0.0)*@PorcBeca)/100)* -1), 0, 1, 1, 100, 1, @DiaB, @MesD, @PagoNumB, @PagosB, 1, @Beca, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente
              END

			  --Agregar Becas a comentarios
			  IF @Comentarios NOT LIKE  '%Beca:%'
				SELECT @Comentarios = @Comentarios + 'Beca: ' + ISNULL(@Beca,'')

			  ELSE IF @Comentarios NOT LIKE '%' + ISNULL(@Beca,'') + '%'
			    SELECT @Comentarios = @Comentarios + ', ' + ISNULL(@Beca,'')

			  --Tabla para facturar becas 
			  SELECT @Unidad = Unidad,
					 @Moneda = ISNULL(NULLIF(MonedaPrecio,''),@ContMoneda)
				FROM Art WHERE Articulo = @ArticuloBeca
							
			  SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda

			  INSERT INTO @TablaBeca (Beca, Cliente, Articulo, Importe, Moneda, TipoCambio, Unidad, PagoNum, Pagos, ConceptoCuota, Causa) 
              VALUES (@Beca, @Cliente, @ArticuloBeca, ((ISNULL(@ImporteBeca,0.0) * @PorcBeca) / 100), @Moneda, @TipoCambio, @Unidad, @PagoNumB, @PagosB, @ConceptoCuotaB, @CausaVenta)
               
			  FETCH NEXT FROM crVentaBeca INTO @ID2D, @RID2, @ArticuloParaAplicarBeca, @ImporteBeca, @DiaB, @MesD, @PagoNumB, @PagosB, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente, @CausaVenta
            END
            CLOSE crVentaBeca
            DEALLOCATE crVentaBeca           
          END
          FETCH NEXT FROM crBecas INTO @Beca
        END 
        CLOSE crBecas
        DEALLOCATE crBecas
      END              
    
	END ELSE IF @FacturaMultiple = 1
    BEGIN

	  IF NOT EXISTS (SELECT * FROM CEAlumnoCxc3 WHERE Alumno = @Alumno AND Movimiento = 1)
        SELECT @Ok = 55115, @OkRef = @OkRef + ' Alumno: ' + @Alumno
            
      DECLARE crCuotasMovAlumCxc CURSOR FOR   
      SELECT Cliente
        FROM CEAlumnoCxc3 
       WHERE Alumno = @Alumno 
		 AND Movimiento = 1 
	   GROUP BY Cliente

      OPEN crCuotasMovAlumCxc
      FETCH NEXT FROM crCuotasMovAlumCxc INTO @ClienteD
      WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
      BEGIN
        SET @TipoCuota = NULL

        SELECT @TipoCuota        = NombreCuota,
			   @ConceptoVentaCte = ConceptoVenta
          FROM CECuotasClientes
         WHERE ConceptoCuota = @ConceptoCuota
		   AND Cliente       = @ClienteD
		   AND Alumno        = @Alumno
               
        IF @TipoCuota IS NOT NULL AND NOT EXISTS (SELECT * FROM CEPECuotasMovNombreCuota WHERE ID = @IDCuota AND  NombreCuota = @TipoCuota)
          SET @TipoCuota = NULL                  

        IF @TipoCuota IS NULL
		BEGIN
          SELECT @TipoCuota = NombreCuota
            FROM CEPECuotasMovNombreCuota
           WHERE ID = @IDCuota 
		     AND PorOmision = 1
		END
     
        SELECT @NumPagos = ISNULL(SUM(Cantidad),1)
          FROM CETiposCuotasD
         WHERE Cuota = @TipoCuota
		   AND Empresa = @Empresa

        SET @Pagos = null    

		--Inicio Obtener el porcentaje de cuota por cliente
		SET @PorcCliente = NULL

        IF (SELECT ISNULL(@ConceptoVentaCte,ConceptoVenta) FROM CEPECuotasMovNombreCuota WHERE ID = @IDCuota AND NombreCuota = @TipoCuota)
		IN (SELECT Cuotas FROM CEAlumnoCxc3 WHERE Alumno = @Alumno AND Cliente = @ClienteD AND Movimiento = 1)	  
		BEGIN
          SELECT @PorcCliente = Porcentaje
			FROM CEAlumnoCxc3 
		   WHERE Alumno = @Alumno 
		     AND Cliente = @ClienteD
		     AND Movimiento = 1
		   AND Cuotas = (SELECT ISNULL(@ConceptoVentaCte,ConceptoVenta)
		                   FROM CEPECuotasMovNombreCuota 
                          WHERE ID = @IDCuota AND NombreCuota = @TipoCuota)
			  
		END ELSE IF 
		NOT EXISTS (SELECT Porcentaje FROM CEAlumnoCxc3 WHERE Alumno = @Alumno AND  Movimiento = 1 AND Porcentaje = 100
		AND Cuotas = (SELECT ISNULL(@ConceptoVentaCte,ConceptoVenta) FROM CEPECuotasMovNombreCuota WHERE ID = @IDCuota AND NombreCuota = @TipoCuota))
		BEGIN
		  SELECT @PorcCliente = Porcentaje
			FROM CEAlumnoCxc3 
		   WHERE Alumno     = @Alumno 
		     AND Cliente    = @ClienteD
		     AND Movimiento = 1
		     AND Cuotas     = 'Por Omisión'
		END
        -- Fin Obtener el porcentaje de cuota por cliente

		INSERT @Tabla2(RID, Cliente, CondicionPago, ConceptoVenta, NombreCuota, CuotaConcepto, MovVenta, Rubro)
        SELECT @IDCuota, @ClienteD, CondicionPago, ISNULL(@ConceptoVentaCte,ConceptoVenta), NombreCuota, @ConceptoCuota, @MovGenerar, @Rubro
          FROM CEPECuotasMovNombreCuota 
         WHERE ID = @IDCuota
		   AND NombreCuota = @TipoCuota            
        
		SELECT @IDTemp = SCOPE_IDENTITY()    
        
		IF @@ERROR <> 0 SELECT @Ok = 1 

		IF @Ok IS NULL AND @PorcCliente IS NOT NULL
        IF @Tipo IN ('Fijo','Vigencia') --Para Facturar a varios Clientes
        BEGIN
                 
          INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
          SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, @PorcCliente, ISNULL(t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @ClienteD, CausaVenta, CausaRecargo
            FROM CEPECuotasMovNombreCuota c
 LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
           WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
              
        END ELSE IF @Tipo = 'Por Credito' --Para Facturar a varios Clientes
        BEGIN
 
          INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
          SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), ISNULL(@CantCreditos,1), 1, @PorcCliente, ISNULL(t.Cantidad,1)*ISNULL(@CantCreditos,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @ClienteD, CausaVenta, CausaRecargo
            FROM CEPECuotasMovNombreCuota c
            LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
           WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
        
		END ELSE IF @Tipo = 'Por Credito Rango' --Para Facturar a varios Clientes
        BEGIN

          SELECT @Cuota = Cuota FROM CEPECuotasMovRangos WHERE ID = @IDCuota AND ISNULL(@CantCreditos,1) BETWEEN De AND Hasta
                    
          INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, Observaciones, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
          SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, @PorcCliente, ISNULL(t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, CONVERT(varchar,ISNULL(@CantCreditos,1))+' Creditos', ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @ClienteD, CausaVenta, CausaRecargo
            FROM CEPECuotasMovNombreCuota c
            LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
           WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
        
		END ELSE IF @Tipo = 'Por Materia' --Para Facturar a varios Clientes
        BEGIN
                
          IF @MovTipo IN ('CE.PRM','CE.RM')
          BEGIN
            DECLARE crCursorMateria CURSOR FOR   
            SELECT Materia
              FROM CED
             WHERE ID = @ID  
              
            OPEN crCursorMateria
            FETCH NEXT FROM crCursorMateria INTO @Materia
            WHILE @@FETCH_STATUS = 0
            BEGIN
              SELECT @ArticuloMov = Articulo FROM CEMateria WHERE Materia = @Materia AND Empresa = @Empresa
              IF ISNULL(@ArticuloMov,'') = ''
                SELECT @ArticuloMov =  Articulo FROM CEPECuotasMovNombreCuota WHERE ID = @IDCuota AND NombreCuota = @TipoCuota 
              SELECT @Moneda = ISNULL(NULLIF(DefMoneda,''),@ContMoneda) FROM Cte WHERE Cliente= @ClienteD
              SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda
              EXEC spPCGet @Sucursal, @Empresa, @ArticuloMov, @SubCuenta, @Unidad, @Moneda, @TipoCambio, @ListaPrecios, @PrecioMateria OUTPUT
                     
              INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente,
 CausaVenta, CausaRecargo)
              SELECT @IDTemp, @IDCuota, @Alumno, ISNULL(@ArticuloMov,c.Articulo), (@PrecioMateria/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, @PorcCliente, ISNULL(
t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota
, ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @ClienteD, CausaVenta, CausaRecargo
                FROM CEPECuotasMovNombreCuota c
                LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa
               WHERE ID = @IDCuota AND NombreCuota = @TipoCuota
                                  
              FETCH NEXT FROM crCursorMateria INTO @Materia
            END
            CLOSE crCursorMateria
            DEALLOCATE crCursorMateria
                
		  END ELSE IF @MovTipo NOT IN ('CE.PRM','CE.RM')
          BEGIN
			SELECT @Moneda = DefMoneda FROM Cte WHERE Cliente = @Cliente
			
			IF @Moneda IS NULL SET @Moneda = @ContMoneda
			
			SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda
				  
			INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta
, CausaRecargo)
			SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (ISNULL(dbo.fnPCGet (@Empresa, @Sucursal, @Moneda,@TipoCambio, c.Articulo, NULL, NULL, @ListaPrecios),0.0)/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, ISNULL(@CantMaterias,1), @PorcCliente, ISNULL(t.Cantidad,1)*ISNULL(@CantMaterias,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @ClienteD, CausaVenta, CausaRecargo
			  FROM CEPECuotasMovNombreCuota c
			  LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa 
			 WHERE ID = @IDCuota AND NombreCuota = @TipoCuota     
          END

        END ELSE IF @Tipo = 'Por Materias Rango' --Para Facturar a varios Clientes
        BEGIN

          SELECT @Cuota = Cuota FROM CEPECuotasMovRangos WHERE ID = @IDCuota AND ISNULL(@CantMaterias,1) BETWEEN De AND Hasta
                
          INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, ConceptoCuota, Observaciones, RecSrec, ConceptoRecargo, MovRecargo, Cliente, CausaVenta, CausaRecargo)
          SELECT @IDTemp, @IDCuota, @Alumno, c.Articulo, (@Cuota/@NumPagos), ISNULL(CASE WHEN (SELECT CENoAplicaPorcMat FROM Art WHERE Articulo = c.Articulo) = 1 THEN 0 ELSE c.Porcentaje END,0), 1, 1, @PorcCliente, ISNULL(t.Cantidad,1), ISNULL(t.Dia,@DiaO), ISNULL(t.Periodo,@MesO), CASE WHEN @BanderaRecargoFijo = 0 THEN c.Recargo ELSE NULL END, CASE WHEN @BanderaRecargoFijo = 1 THEN c.RecargoFijo ELSE NULL END, c.RecargoTope, c.ArticuloRecargo, c.Frecuencia, @ConceptoCuota, CONVERT(varchar,ISNULL(@CantMaterias,1))+' Materias', ISNULL(c.RecSRec,0), c.ConceptoRecargo, @MovRecargoPlan, @ClienteD, CausaVenta, CausaRecargo
            FROM CEPECuotasMovNombreCuota c
            LEFT JOIN CETiposCuotasD t ON t.Cuota = c.NombreCuota AND t.Empresa = @Empresa 
           WHERE ID = @IDCuota AND NombreCuota = @TipoCuota        
        END
        
		IF @Ok IS NULL AND EXISTS (SELECT * FROM @Tabla2D)
		BEGIN
		  SELECT @Pagos = COUNT(*) FROM @Tabla2D WHERE ID = @IDTemp               
          
		  SET @NumPago = 0
          
		  UPDATE @Tabla2D
             SET @Numpago = PagoNum = @Numpago + 1,
			     Pagos    = @Pagos
            FROM @Tabla2D
           WHERE ID = @IDTemp
		END
				
		-- Becas para Facturar a varios Clientes  
        IF @Ok IS NULL AND @Rubro IS NOT NULL AND @PorcCliente IS NOT NULL
		AND EXISTS (SELECT * FROM CEBecasActivas WHERE Empresa = @Empresa AND CicloEscolar = @CicloEscolar AND Programa = @Programa AND Alumno = @Alumno) 
		BEGIN
          -- Modificado por: RG 30/03/2015, Seleccionar solo las becas de la vista Becas Activas
          DECLARE crBecas CURSOR FOR
          SELECT Beca
            FROM CEBecasActivas  
           WHERE Empresa = @Empresa
			 AND Alumno = @Alumno
			 AND CicloEscolar = @CicloEscolar
			 AND Programa = @Programa
    
          OPEN crBecas
          FETCH NEXT FROM crBecas INTO @Beca
          WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
          BEGIN 
            SELECT @ArticuloBeca = Articulo,
				   @PorcBeca = PorcdeDesc,
				   @CubreTodo = CubreTodo,
				   @CubreAdmision = CubreAdmision,
				   @CubreInscripcion = CubreInscripcion,
				   @CubreColegiatura = CubreColegiatura,
				   @CubreCuotasAdicionales = CubreCuotasAdicionales,
				   @CubreServicioSocial = CubreServicioSocial,
				   @CubreTitulacion = CubreTitulacion,
				   @CubreExtraordinario  = CubreExtraordinario,
				   @CubreOtros = CubreOtros              
              FROM CEBecaCiclo
             WHERE Empresa = @Empresa
			   AND Ciclo = @CicloEscolar
			   AND Beca = @Beca 
             
            IF @CubreTodo = 1
			  OR @Rubro = 'Admisión'               AND @CubreAdmision = 1
			  OR @Rubro = 'Inscripción'            AND @CubreInscripcion = 1
			  OR @Rubro = 'Colegiatura'            AND @CubreColegiatura = 1
			  OR @Rubro = 'Cuotas Adicionales'     AND @CubreCuotasAdicionales = 1
			  OR @Rubro = 'Servicio Social'        AND @CubreServicioSocial = 1
			  OR @Rubro = 'Titulación'             AND @CubreTitulacion = 1
			  OR @Rubro = 'Examen Extraordinarios' AND @CubreExtraordinario = 1
			  OR @Rubro = 'Otros'                  AND @CubreOtros = 1
			BEGIN

              DECLARE crVentaBeca CURSOR FOR   
              SELECT ID,
					 RID,
					 Articulo,
					 PorcCliente * (Importe * Cantidad + (Porcentaje * (Importe*Cantidad) / 100)) / 100,
					 Dia,
					 Mes,
					 PagoNum,
					 Pagos,
					 ConceptoCuota,
					 PorcRecargo,
					 RecargoFijo,
					 RecargoTope,
					 ArtRecargo,
					 Frecuencia,
					 RecSRec,
					 ConceptoRecargo,
					 MovRecargo,
					 Cliente,
					 CausaVenta
                FROM @Tabla2D 
               WHERE ID = @IDTemp
				 AND ISNULL(EsBeca,0) = 0
				 AND Articulo NOT IN (SELECT Articulo FROM Art WHERE CENoAplicaBeca = 1)
                      
              OPEN crVentaBeca
			  FETCH NEXT FROM crVentaBeca INTO @ID2D, @RID2, @ArticuloParaAplicarBeca, @ImporteBeca, @DiaB, @MesD, @PagoNumB, @PagosB, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente, @CausaVenta
			  WHILE (@@FETCH_STATUS = 0)
			  BEGIN
                        
				IF (@BecasCascada = 1)
				BEGIN
				  IF (@CalcularVenta <> 'Cuotas Kardex')
				  BEGIN
					UPDATE @Tabla2D
					   SET DescuentoLinea = isnull(DescuentoLinea,1) * 
						   CASE 
						     WHEN @PorcBeca = 100
							 THEN 100
							 ELSE (1-( cast(@PorcBeca as float) /100))
						   END,
						   --PorcCliente = 100,
						   Beca = @Beca
					 WHERE ID = @ID2D
					   AND RID = @RID2
					   AND Articulo = @ArticuloParaAplicarBeca
					   AND Cliente = @ClienteD
                      
				END ELSE
				  IF (((ISNULL(@ImporteBeca + (SELECT MIN(Importe) FROM @Tabla2D WHERE Importe < 0 AND ID = @ID2D),0.0)*@PorcBeca)/100)*-1) = 0 
					INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PagoNum, Pagos, EsBeca, Beca, ConceptoCuota, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, RecSRec, ConceptoRecargo,
 MovRecargo)
					SELECT @ID2D, @RID2, @Alumno, @ArticuloBeca, (((ISNULL(@ImporteBeca,0.0)*@PorcBeca)/100)*-1), 0, 1, 1, 100, 1, @DiaB, @MesD, @PagoNumB, @PagosB, 1, @Beca, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB
				  ELSE
					INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PagoNum, Pagos, EsBeca, Beca, ConceptoCuota, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, RecSRec, ConceptoRecargo,
 MovRecargo)
					SELECT @ID2D, @RID2, @Alumno, @ArticuloBeca, (((ISNULL(@ImporteBeca + (SELECT SUM(Importe) FROM @Tabla2D WHERE Importe < 0 AND ID = @ID2D),0.0)*@PorcBeca)/100)*-1), 0, 1, 1, 100, 1, @DiaB, @MesD, @PagoNumB, @PagosB, 1, @Beca, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB
                     
				END ELSE 
				BEGIN
								  
				  IF @CalcularVenta <> 'Cuotas Kardex'
				  BEGIN
					UPDATE @Tabla2D
					   SET DescuentoLinea = ISNULL(DescuentoLinea,0) + @PorcBeca,
					  	   --PorcCliente = 100,
					  	   Beca = @Beca
					 WHERE ID = @ID2D
					   AND RID = @RID2
					   AND Articulo = @ArticuloParaAplicarBeca
					   AND Dia = @DiaB
					   AND mes = @MesD
					   AND PagoNum = @PagoNumB
					   AND Cliente = @Cliente
					  			
				   END ELSE BEGIN
					 INSERT @Tabla2D(ID, RID, Alumno, Articulo, Importe, Porcentaje, Creditos, Materias, PorcCliente, Cantidad, Dia, Mes, PagoNum, Pagos, EsBeca, Beca, ConceptoCuota, PorcRecargo, RecargoFijo, RecargoTope, ArtRecargo, Frecuencia, RecSRec, ConceptoRecargo
, MovRecargo)
					 SELECT @ID2D, @RID2, @Alumno, @ArticuloBeca, (((ISNULL(@ImporteBeca,0.0) * @PorcBeca) / 100) * -1), 0, 1, 1, 100, 1, @DiaB, @MesD, @PagoNumB, @PagosB, 1, @Beca, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB
, @RecSRecB, @ConceptoRecargoB, @MovRecargoB
				  END
                END

				--Agregar Becas a comentarios
				IF @Comentarios NOT LIKE  '%Beca:%'
				  SELECT @Comentarios = @Comentarios + 'Beca: ' + ISNULL(@Beca,'')

				ELSE IF @Comentarios NOT LIKE '%' + ISNULL(@Beca,'') + '%'
				  SELECT @Comentarios = @Comentarios + ', ' + ISNULL(@Beca,'')

				--Tabla para facturar becas 
				SELECT @Unidad = Unidad,
				  	   @Moneda = ISNULL(NULLIF(MonedaPrecio,''),@ContMoneda)
			 	  FROM Art WHERE Articulo = @ArticuloBeca

				SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda

				INSERT INTO @TablaBeca (Beca, Cliente, Articulo, Importe, Moneda, TipoCambio, Unidad, PagoNum, Pagos, ConceptoCuota, Causa) 
                VALUES (@Beca, @Cliente, @ArticuloBeca, ((ISNULL(@ImporteBeca,0.0) * @PorcBeca) / 100), @Moneda, @TipoCambio, @Unidad, @PagoNumB, @PagosB, @ConceptoCuotaB, @CausaVenta)  
                    
				FETCH NEXT FROM crVentaBeca INTO @ID2D, @RID2, @ArticuloParaAplicarBeca, @ImporteBeca, @DiaB, @MesD, @PagoNumB, @PagosB, @ConceptoCuotaB, @PorcRecargoB, @RecargoFijoB, @RecargoTopeB, @ArtRecargoB, @FrecuenciaB, @RecSRecB, @ConceptoRecargoB, @MovRecargoB, @Cliente, @CausaVenta
              END
              CLOSE crVentaBeca
              DEALLOCATE crVentaBeca           
            END
            FETCH NEXT FROM crBecas INTO @Beca
          END 
          CLOSE crBecas
          DEALLOCATE crBecas 
        END              
        FETCH NEXT FROM crCuotasMovAlumCxc INTO @ClienteD
      END
      CLOSE crCuotasMovAlumCxc
      DEALLOCATE crCuotasMovAlumCxc  
    END     
    FETCH NEXT FROM crCuotasMov INTO @IDCuota, @Tipo, @Cuota, @ConceptoCuota, @Rubro
  END
  CLOSE crCuotasMov
  DEALLOCATE crCuotasMov

  -- Agregado por RG 05/12/2017; Hacer el cálculo de descuento de línea basado en las becas
  IF (@BecasCascada = 1) AND EXISTS (SELECT DescuentoLinea FROM @Tabla2D)
	UPDATE @Tabla2D SET DescuentoLinea = (1 - (ISNULL(DescuentoLinea,1))) * 100 WHERE ISNULL(DescuentoLinea,0) > 0 AND DescuentoLinea <> 100

  IF @Ok IS NULL AND EXISTS (SELECT * FROM @Tabla2 WHERE NombreCuota <> '(Unico)')
  BEGIN
    --select 'Aplica becas' --iggr
    DECLARE crVentaNoUnico CURSOR FOR
    SELECT Cliente, ConceptoVenta, CondicionPago, MovVenta, NombreCuota
      FROM @Tabla2  
     WHERE NombreCuota <> '(Unico)' 
     GROUP BY Cliente, ConceptoVenta, CondicionPago, MovVenta, NombreCuota
   
    OPEN crVentaNoUnico
    FETCH NEXT FROM crVentaNoUnico INTO @ClienteV, @ConceptoVenta, @CondicionPago, @MovGenerarD, @TipoCuota
    WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
    BEGIN
				  
      IF @Ok IS NULL
      BEGIN     
	                                                            
		--Anteriormente se consultaba en los movimientos por omisión de la vertical el movimiento configurado en Plan de Pagos
        --SELECT @MovGenerar = CEPlanPagos, @EstatusD = 'PENDIENTE' FROM EmpresaCfgMovCE WHERE Empresa = @Empresa
		--Ahora se toma en cuenta el Movimiento que esta configurado en Configurar > Tipos de Movimiento > CE > Pestaña CE
        
        SELECT @Moneda = ISNULL(NULLIF(DefMoneda,''),@ContMoneda) FROM Cte WHERE Cliente = @ClienteV

        SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda

        IF @Ok IS NULL
        BEGIN
          DECLARE crVentaNoUnicoD CURSOR FOR 
          SELECT Articulo,
				 Importe,
				 DescuentoLinea,
				 Porcentaje,
				 Creditos,
				 Materias,
				 PorcCliente,
				 Cantidad,
				 Dia,
				 Mes,
				 PorcRecargo,
				 RecargoFijo,
				 RecargoTope,
				 ArtRecargo,
				 Frecuencia,
				 PagoNum,
				 Pagos,
				 Beca,
				 ConceptoCuota,
				 Observaciones,
				 RecSRec,
				 ConceptoRecargo,
				 MovRecargo,
				 CausaVenta,
				 CausaRecargo
            FROM @Tabla2D
           WHERE ID IN (SELECT ID FROM @Tabla2
						 WHERE Cliente = @ClienteV AND ConceptoVenta = @ConceptoVenta
			               AND CondicionPago = @CondicionPago AND NombreCuota <> '(Unico)')
          
          SELECT @Renglon = 2048.0, @RenglonID = 1

          OPEN crVentaNoUnicoD
          FETCH NEXT FROM crVentaNoUnicoD INTO @ArticuloV, @ImporteV, @DescuentoLinea, @PorcentajeV, @CreditosV, @MateriasV, @PorcCliente, @CantidadV, @DiaV, @MesV, @PorcRecargo, @RecargoFijo, @RecargoTope, @ArtRecargo, @Frecuencia, @PagoNumV, @PagosV, @BecaV, @ConceptoCuotaVD, @Observaciones, @RecSRec2, @ConceptoRecargo, @MovRecargo, @CausaVenta, @CausaRecargo
          WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
          BEGIN

			-- Validar causa en Venta
			IF ISNULL(@CausaVenta,'') = '' AND @CalcularVenta <> 'Cuotas Kardex'
			  SELECT @Ok = 1000214

			SELECT @NumDocumento = CONVERT(varchar,ISNULL(@PagoNumV,1))+' de '+CONVERT(varchar,ISNULL(@PagosV,1))
 
            SELECT @Fecha2 = DATEADD(month, @MesV-1, @FechaCiclo) 
			
			-- Convertir el dia a 28 cuando el mes es febrero para evitar errores con el 29 de febrero
			IF DATEPART(MONTH,@Fecha2) = 2 AND @DiaV > 28 
			  SELECT @DiaV = 28
			ELSE IF ISNULL(@DiaV,0) = 0
			  SELECT @DiaV = 1

            SELECT @Fecha = CONVERT(datetime,CONVERT(varchar,@DiaV)+'/'+CONVERT(varchar,DATEPART(MONTH,@Fecha2))+'/'+CONVERT(varchar,DATEPART(YEAR,@Fecha2)))
			
            SELECT @ZonaImpuesto = ZonaImpuesto FROM Cte WHERE Cliente = @ClienteV
            SELECT @ImporteV = (@ImporteV)
            SELECT @Precio = @ImporteV +((@PorcentajeV*@ImporteV)/100)
            SELECT @Precio = (@PorcCliente*@Precio)/100
            
			SELECT @ArtTipo   = Tipo,
			       @Impuesto1 = Impuesto1,
			  	   @Impuesto2 = Impuesto2,
				   @Impuesto3 = Impuesto3,
				   @Unidad = Unidad,
				   @Moneda = ISNULL(NULLIF(MonedaPrecio,''),@ContMoneda)
	  		  FROM Art
			 WHERE Articulo = @ArticuloV
            
			EXEC spRenglonTipo @ArtTipo, NULL, @RenglonTipo OUTPUT  
            EXEC spZonaImp @ZonaImpuesto, @Impuesto1 OUTPUT
            EXEC spZonaImp @ZonaImpuesto, @Impuesto2 OUTPUT
            EXEC spZonaImp @ZonaImpuesto, @Impuesto3 OUTPUT
            EXEC spTipoImpuesto 'VTAS', @IDGenerar, @MovGenerar, @FechaEmision, @Empresa, 0, @ClienteV, NULL, @Articulo = @ArticuloV, @EnSilencio = 1, @Impuesto1 = @Impuesto1 OUTPUT, @Impuesto2 = @Impuesto2 OUTPUT, @Impuesto3 = @Impuesto3 OUTPUT
            
            SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda

			-- Agregar el número de documento a comentarios
			IF @Comentarios NOT LIKE '%Cuota Movimiento%' AND @Comentarios LIKE '%Beca:%'
			  SELECT @Comentarios = @Comentarios + CHAR(13)

			IF @Comentarios NOT LIKE '%Cuota Movimiento%'
			  SELECT @Comentarios = @Comentarios + 'Cuota Movimiento ' + @TipoCuota + ' - Número ' + @NumDocumento + CHAR(13)
			
			ELSE
			  SELECT @Comentarios = SUBSTRING(@Comentarios,0,CHARINDEX('Cuota Movimiento',@Comentarios)) + 'Cuota Movimiento ' + @TipoCuota + ' - Número ' + @NumDocumento + CHAR(13)
            
			-- Si en el catálogo de alumnos esta prendido el check de No Generar Recargos no se indica que se generan recargos
			IF @NoGenerarRecargos = 0 AND ISNULL(@ArtRecargo,'') <> '' AND ISNULL(@Frecuencia,'') <> '' 
			AND (ISNULL(@PorcRecargo,0) > 0 OR ISNULL(@RecargoFijo,0) > 0)
			  SELECT @Comentarios = @Comentarios + 'Generar Recargos después de la Fecha de Vencimiento'

			-- Inicio generar encabezado de venta <> 'Unico'
			IF @OK IS NULL AND @CalcularVenta <> 'Cuotas Kardex'
			BEGIN
			  INSERT Venta (
					  Empresa, Mov, FechaEmision, Concepto, Usuario, Moneda, TipoCambio, Condicion, Estatus, Directo, 
					  Prioridad, Cliente, Almacen, Sucursal, OrigenTipo, Origen, OrigenID, DocFuente, ContUso, Referencia, UEN,
					  FechaRequerida, Causa, Comentarios, Observaciones)
			  VALUES (@Empresa, @MovGenerar, @FechaEmision, @ConceptoVenta, @Usuario, @Moneda, @TipoCambio, @CondicionPago, 'SINAFECTAR', 1, 
					  'Normal', @ClienteV, @Almacen, @Sucursal, 'CE', @Mov, @MovID, @ID, @CentroCosto, @AlumnoRefVTAS, @UEN,
					  @Fecha, @CausaVenta, @Comentarios, @AlumnoComVTAS)
					    
		
			END ELSE BEGIN
			  INSERT @CuotasKardex (
					  Empresa, Mov, FechaEmision, Concepto, Moneda, TipoCambio, Usuario, Condicion, Estatus, Directo, Prioridad,
					  Cliente, Almacen, FechaRequerida, Vencimiento, Sucursal, OrigenTipo, Origen, OrigenID, DocFuente, ContUso, Referencia)
			  VALUES (@Empresa, @MovGenerar, @FechaEmision, @ConceptoVenta, @Moneda, @TipoCambio, @Usuario, @CondicionPago, 'SINAFECTAR', 1, 'Normal',
					  @ClienteV, @Almacen, @FechaEmision, @FechaEmision, @Sucursal, 'CE', @Mov, @MovID, @ID, @CentroCosto, @AlumnoRefVTAS)
					
					  
			END
       
			IF @@ERROR <> 0 SELECT @Ok = 1

			SET @IDGenerar = SCOPE_IDENTITY()
            
            IF @OK IS NULL AND @IDGenerar IS NOT NULL AND @CalcularVenta <> 'Cuotas Kardex'  
            BEGIN

			  INSERT VentaD (
					  ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, CantidadPendiente, Unidad, Almacen, Articulo,
					  Precio, PrecioMoneda, PrecioTipoCambio, FechaRequerida, Sucursal, CantidadInventario, Impuesto1, Impuesto2, Impuesto3,
					  DescripcionExtra, ContUso, DescuentoLinea)
			  VALUES (@IDGenerar, @Renglon, 0, @RenglonID, @RenglonTipo, @CantidadV, NULL, @Unidad, @Almacen, @ArticuloV, 
					  @Precio, @Moneda, @TipoCambio, @Fecha, @Sucursal, @CantidadV, @Impuesto1, @Impuesto2, @Impuesto3,
					  convert (varchar(12),  @Fecha,103)+'111', @CentroCosto, @DescuentoLinea)
					   
			  IF @@ERROR <> 0 SELECT @Ok = 1  

			  IF @OK IS NULL AND @CalcularVenta = 'Movimiento'
				EXEC spAfectar 'VTAS', @IDGenerar, 'AFECTAR', 'Todo', NULL, @Usuario, @EnSilencio = 1, @Conexion = 1, @Ok = @Ok OUTPUT, @OkRef = @OkRef OUTPUT
             
			  ELSE IF @OK IS NULL AND @CalcularVenta = 'Job'
				EXEC spAfectar 'VTAS', @IDGenerar, 'AFECTAR', 'Todo', NULL, @Usuario, @EnSilencio = 1, @Estacion = 2 
              
			  IF @OK IS NULL
			  BEGIN
				SELECT @MovIDGenerar = MovID FROM Venta WHERE ID = @IDGenerar

				EXEC spMovFlujo @Sucursal, 'AFECTAR', @Empresa, 'CE', @ID, @Mov, @MovID, 'VTAS', @IDGenerar, @MovGenerar, @MovIDGenerar, @Ok OUTPUT       
			  END

			  IF @OK IS NULL AND @MovTipo IN ('CE.IE','CE.CEX') 
				UPDATE CEInfoAlumno SET FechaUltimaCuotaIE = GETDATE() WHERE IDMov = @ID
				 
            END ELSE BEGIN
              INSERT @CuotasKardexD (
					  ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, CantidadPendiente, Unidad, Almacen, Articulo, Precio,
					  FechaRequerida, Sucursal, CantidadInventario, Alumno, FechaVencimiento, PorcRecargo, ArtRecargo, Frecuencia, MovGenerar,
					  EsBeca, Beca, EstatusD, Impuesto1, Impuesto2, Impuesto3, ConceptoCuota,
					  NumDocumento, DescripcionExtra, ContUso, RecSrec)
              VALUES (@IDGenerar, @Renglon, 0, @RenglonID, @RenglonTipo, @CantidadV, NULL, @Unidad, @Almacen, @ArticuloV, @Precio,
					  @Fecha, @Sucursal, @CantidadV, @Alumno, @Fecha, @PorcRecargo, @ArtRecargo, @Frecuencia, @MovGenerarD,
					  0, @BecaV, @EstatusD, @Impuesto1, @Impuesto2, @Impuesto3, @ConceptoCuotaVD,
					  @NumDocumento, @Fecha, @CentroCosto, @RecSrec)
					 
            END

			--IGGR. Si los movimientos son de tipo Inscripcion Express utilizara un procedimiento almacenado que permita aplicar Becas
			  --al plan de estudios de Enfermeria
			  IF @MovTipo IN ('CE.IE') AND @Ok IS NULL AND @IDGenerar IS NOT NULL
			 	   EXEC xpFBHAplicarBecaEnferm @ID,@IDGenerar,@Renglon

            IF @@ERROR <> 0 SELECT @Ok = 1     
            
			SELECT @Renglon = @Renglon + 2048.0, @RenglonID = @RenglonID + 1
			-- Fin generar detalle de venta <> 'Unico'
               
            FETCH NEXT FROM crVentaNoUnicoD INTO @ArticuloV, @ImporteV, @DescuentoLinea, @PorcentajeV, @CreditosV, @MateriasV, @PorcCliente, @CantidadV, @DiaV, @MesV, @PorcRecargo, @RecargoFijo, @RecargoTope, @ArtRecargo, @Frecuencia, @PagoNumV, @PagosV, 
@BecaV, @ConceptoCuotaVD, @Observaciones, @RecSRec2, @ConceptoRecargo, @MovRecargo, @CausaVenta, @CausaRecargo
          END
          CLOSE crVentaNoUnicoD
          DEALLOCATE crVentaNoUnicoD
        END
      END   
               
      FETCH NEXT FROM crVentaNoUnico INTO @ClienteV, @ConceptoVenta, @CondicionPago, @MovGenerarD, @TipoCuota
    END 
    CLOSE crVentaNoUnico
    DEALLOCATE crVentaNoUnico
  END   
    	
  IF @Ok IS NULL AND EXISTS (SELECT * FROM @Tabla2 WHERE NombreCuota = '(Unico)') 
  BEGIN

	-- Agregado por RG; Se agerego esta variable porque cuando se tenian más de una cuota configurada con la misma CondicionPago y Moviento se agrupaban en una misma factura y en las demas salian sin detalle
    DECLARE @RidVentaD int
       
    DECLARE crVentaUnico CURSOR FOR
    SELECT Cliente, ConceptoVenta, CondicionPago, MovVenta, ID, NombreCuota
      FROM @Tabla2  
     WHERE NombreCuota = '(Unico)' 
     GROUP BY Cliente, ConceptoVenta, CondicionPago, MovVenta, ID, NombreCuota

    OPEN crVentaUnico
    FETCH NEXT FROM crVentaUnico INTO @ClienteV, @ConceptoVenta, @CondicionPago, @MovGenerar, @RidVentaD, @TipoCuota
    WHILE (@@FETCH_STATUS = 0) AND (@Ok IS NULL)
    BEGIN

      IF @Ok IS NULL
      BEGIN
        SELECT @Moneda = ISNULL(NULLIF(DefMoneda,''),@ContMoneda) FROM Cte WHERE Cliente = @ClienteV
        SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda
        
        
        IF @CalcularVenta <> 'Cuotas Kardex'
		BEGIN
          INSERT Venta (
				  Empresa, Mov, FechaEmision, Concepto, Usuario, Moneda, TipoCambio, Condicion, Estatus, Directo, 
				  Prioridad, Cliente, Almacen, Sucursal, OrigenTipo, Origen, OrigenID, DocFuente, ContUso, Referencia, UEN, Observaciones)
          VALUES (@Empresa, @MovGenerar, @FechaEmision, @ConceptoVenta, @Usuario, @Moneda, @TipoCambio, @CondicionPago, 'SINAFECTAR', 1, 
		          'Normal', @ClienteV, @Almacen, @Sucursal, 'CE', @Mov, @MovID, @ID, @CentroCosto, @AlumnoRefVTAS, @UEN, @AlumnoComVTAS)
				  
        END ELSE BEGIN
          INSERT @CuotasKardex (
			      Empresa, Mov, FechaEmision, Concepto, Moneda, TipoCambio, Usuario, Condicion, Estatus, Directo, Prioridad,
			 	  Cliente, Almacen, FechaRequerida, Vencimiento, Sucursal, OrigenTipo, Origen, OrigenID, DocFuente, ContUso, Referencia)
          VALUES (@Empresa, @MovGenerar, @FechaEmision, @ConceptoVenta, @Moneda, @TipoCambio, @Usuario, @CondicionPago, 'SINAFECTAR', 1, 'Normal',
				  @ClienteV, @Almacen, @FechaEmision, @FechaEmision, @Sucursal, 'CE', @Mov, @MovID, @ID, @CentroCosto, @AlumnoRefVTAS)
        
	    END

        IF @@ERROR <> 0 SELECT @Ok = 1
        
		SET @IDGenerar = SCOPE_IDENTITY()
          
        IF @Ok IS NULL
        BEGIN
		 
          DECLARE crVentaUnicoD CURSOR FOR 
          SELECT Articulo,
			     Importe,
				 DescuentoLinea,
				 Porcentaje,
				 Creditos,
				 Materias,
				 PorcCliente,
				 Cantidad,
				 Dia,
				 Mes,
				 PorcRecargo,
				 RecargoFijo,
				 RecargoTope,
				 ArtRecargo,
				 Frecuencia,
				 PagoNum,
				 Pagos,
				 Beca,
				 ConceptoCuota,
				 Observaciones,
				 RecSRec,
				 ConceptoRecargo,
				 MovRecargo,
				 CausaVenta,
				 CausaRecargo
            FROM @Tabla2D
           WHERE ID = @RidVentaD
             
          SET @Renglon = 2048.0
          SET @RenglonID = 1
       
          OPEN crVentaUnicoD
          FETCH NEXT FROM crVentaUnicoD INTO @ArticuloV, @ImporteV, @DescuentoLinea, @PorcentajeV, @CreditosV, @MateriasV, @PorcCliente, @CantidadV, @DiaV, @MesV, @PorcRecargo, @RecargoFijo, @RecargoTope, @ArtRecargo, @Frecuencia, @PagoNumV, @PagosV, @BecaV, @ConceptoCuotaVD, @Observaciones, @RecSRec2, @ConceptoRecargo, @MovRecargo, @CausaVenta, @CausaRecargo
          WHILE @@FETCH_STATUS = 0 AND @Ok IS NULL
          BEGIN

			-- Validar causa en Venta
			IF ISNULL(@CausaVenta,'') = '' AND @CalcularVenta <> 'Cuotas Kardex'
			  SELECT @Ok = 1000214
			 
			SELECT @NumDocumento = CONVERT(varchar,ISNULL(@PagoNumV,1))+' de '+CONVERT(varchar,ISNULL(@PagosV,1))

            SELECT @Fecha2 = DATEADD(month, @MesV, @FechaCiclo) 
            
			-- Convertir el dia a 28 cuando el mes es febrero para evitar errores con el 29 de febrero
			
			IF DATEPART(MONTH,@Fecha2) = 2 AND @DiaV > 28 
			  SELECT @DiaV = 28
			ELSE IF ISNULL(@DiaV,0) = 0
			  SELECT @DiaV = 1

            SELECT @Fecha = CONVERT(datetime,CONVERT(varchar,@DiaV)+'/'+CONVERT(varchar,DATEPART(MONTH,@Fecha2))+'/'+CONVERT(varchar,DATEPART(YEAR,@Fecha2)))
            
			SELECT @ZonaImpuesto = ZonaImpuesto FROM Cte WHERE Cliente = @ClienteV
            SELECT @Precio = @ImporteV +((@PorcentajeV*@ImporteV)/100)
            SELECT @Precio = (@PorcCliente*@Precio)/100
            
			SELECT @ArtTipo   = Tipo,
			       @Impuesto1 = Impuesto1,
			  	   @Impuesto2 = Impuesto2,
				   @Impuesto3 = Impuesto3,
				   @Unidad    = Unidad,
				   @Moneda    = ISNULL(NULLIF(MonedaPrecio,''),@ContMoneda)
			  FROM Art
			 WHERE Articulo = @ArticuloV
            
			EXEC spRenglonTipo @ArtTipo, NULL, @RenglonTipo OUTPUT  
            EXEC spZonaImp @ZonaImpuesto, @Impuesto1 OUTPUT
            EXEC spZonaImp @ZonaImpuesto, @Impuesto2 OUTPUT
            EXEC spZonaImp @ZonaImpuesto, @Impuesto3 OUTPUT
            EXEC spTipoImpuesto 'VTAS', @IDGenerar, @MovGenerar, @FechaEmision, @Empresa, 0, @ClienteV, NULL, @Articulo = @ArticuloV, @EnSilencio = 1, @Impuesto1 = @Impuesto1 OUTPUT, @Impuesto2 = @Impuesto2 OUTPUT, @Impuesto3 = @Impuesto3 OUTPUT
                        
			SELECT @TipoCambio = TipoCambio FROM Mon WHERE Moneda = @Moneda
            
            IF (@CalcularVenta <> 'Cuotas Kardex')
            BEGIN
			  IF @IDGenerar IS NOT NULL
			  BEGIN
				INSERT VentaD (
						ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, CantidadPendiente, Unidad, Almacen, Articulo, 
						Precio, PrecioMoneda, PrecioTipoCambio, FechaRequerida,  Sucursal, CantidadInventario, 
						Impuesto1, Impuesto2, Impuesto3, DescripcionExtra, ContUso, DescuentoLinea)
				VALUES (@IDGenerar, @Renglon, 0, @RenglonID, @RenglonTipo, @CantidadV, NULL, @Unidad, @Almacen, @ArticuloV, 
						@Precio, @Moneda, @TipoCambio, @Fecha, @Sucursal, @CantidadV, 
						@Impuesto1, @Impuesto2, @Impuesto3, convert (varchar(12),  @Fecha,103)+' 112', @CentroCosto, @DescuentoLinea)
               
				-- Agregar el número de documento a comentarios
				IF @Comentarios NOT LIKE '%Cuota Movimiento%' AND @Comentarios LIKE '%Beca:%'
				  SELECT @Comentarios = @Comentarios + CHAR(13)

				IF @Comentarios NOT LIKE '%Cuota Movimiento%'
				  SELECT @Comentarios = @Comentarios + 'Cuota Movimiento ' + @TipoCuota + ' - Número ' + @NumDocumento + CHAR(13)
			
				ELSE
				  SELECT @Comentarios = SUBSTRING(@Comentarios,0,CHARINDEX('Cuota Movimiento',@Comentarios)) + 'Cuota Movimiento ' + @TipoCuota + ' - Número ' + @NumDocumento + CHAR(13)
                
				-- Si en el catálogo de alumnos esta prendido el check de No Generar Recargos no se indica que se generan recargos
				IF @NoGenerarRecargos = 0 AND ISNULL(@ArtRecargo,'') <> '' AND ISNULL(@Frecuencia,'') <> '' 
				AND (ISNULL(@PorcRecargo,0) > 0 OR ISNULL(@RecargoFijo,0) > 0)
				  SELECT @Comentarios = @Comentarios + 'Generar Recargos después de la Fecha de Vencimiento'
				  
				UPDATE Venta
				   SET FechaRequerida = @Fecha,
				       Moneda         = @Moneda,
					   TipoCambio     = @TipoCambio,
					   Causa          = @CausaVenta,
					   Comentarios    = @Comentarios
				 WHERE ID = @IDGenerar 
			  END
                                 
            END ELSE BEGIN
              INSERT @CuotasKardexD (
					  ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, CantidadPendiente, Unidad, Almacen, Articulo, Precio,
					  FechaRequerida, Sucursal, CantidadInventario, Alumno, FechaVencimiento, PorcRecargo, ArtRecargo, Frecuencia, MovGenerar,
					  EsBeca, Beca, EstatusD, Impuesto1, Impuesto2, Impuesto3, ConceptoCuota,
					  NumDocumento, DescripcionExtra, ContUso, RecSrec)
              VALUES (@IDGenerar, @Renglon, 0, @RenglonID, @RenglonTipo, @CantidadV, NULL, @Unidad, @Almacen, @ArticuloV, @Precio,
				      @Fecha, @Sucursal, @CantidadV, @Alumno, @Fecha, @PorcRecargo, @ArtRecargo, @Frecuencia, @MovGenerarD,
				  	  0, @BecaV, @EstatusD, @Impuesto1, @Impuesto2, @Impuesto3, @ConceptoCuotaVD,
					  @NumDocumento, convert (varchar(12),  @Fecha,103)+'666' , @CentroCosto, @RecSrec2)
					  
            END
			   
            IF @@ERROR <> 0 SELECT @Ok = 1     
            
			SELECT @Renglon = @Renglon + 2048.0, @RenglonID = @RenglonID + 1
			
               
            FETCH NEXT FROM crVentaUnicoD INTO @ArticuloV, @ImporteV, @DescuentoLinea, @PorcentajeV, @CreditosV, @MateriasV, @PorcCliente, @CantidadV, @DiaV, @MesV, @PorcRecargo, @RecargoFijo, @RecargoTope, @ArtRecargo, @Frecuencia, @PagoNumV, @PagosV, @BecaV, @ConceptoCuotaVD, @Observaciones, @RecSRec2, @ConceptoRecargo, @MovRecargo, @CausaVenta, @CausaRecargo
          END
          CLOSE crVentaUnicoD
          DEALLOCATE crVentaUnicoD
        END
      END   
          
      IF @Ok IS NULL AND @IDGenerar IS NOT NULL AND @CalcularVenta <> 'Cuotas Kardex'
      BEGIN
	    
            
		IF @CalcularVenta = 'Movimiento'
		  EXEC spAfectar 'VTAS', @IDGenerar, 'AFECTAR', 'Todo', NULL, @Usuario, @EnSilencio = 1, @Conexion = 1, @Ok = @Ok OUTPUT, @OkRef = @OkRef OUTPUT
            
		ELSE IF @CalcularVenta = 'Job'
		  EXEC spAfectar 'VTAS', @IDGenerar, 'AFECTAR', 'Todo', NULL, @Usuario, @EnSilencio = 1, @Estacion = 2
          
        SELECT @MovIDGenerar = MovID FROM Venta WHERE ID = @IDGenerar

        EXEC spMovFlujo @Sucursal, 'AFECTAR', @Empresa, 'CE', @ID, @Mov, @MovID, 'VTAS', @IDGenerar, @MovGenerar, @MovIDGenerar, @Ok OUTPUT       
          
        IF @MovTipo IN ('CE.IE','CE.CEX') 
        UPDATE CEInfoAlumno SET FechaUltimaCuotaIE = GETDATE() WHERE IDMov = @ID 
	
      END
        
      FETCH NEXT FROM crVentaUnico INTO @ClienteV, @ConceptoVenta, @CondicionPago, @MovGenerar, @RidVentaD, @TipoCuota
    END 
    CLOSE crVentaUnico
    DEALLOCATE crVentaUnico
  END       
     
  -- Facturar Becas 
  IF @Ok IS NULL
  AND EXISTS (SELECT * FROM @TablaBeca a JOIN CEBecaCiclo b ON a.Beca = b.Beca AND b.Empresa = @Empresa AND b.Ciclo = @CicloEscolar AND b.FormaBeca = 'Facturable') 
  BEGIN
     
    DECLARE crBecaUnico CURSOR FOR 
    SELECT a.Beca, a.Cliente, a.Moneda, a.TipoCambio, a.Unidad, a.Causa
      FROM @TablaBeca a
	  JOIN CEBecaCiclo b ON a.Beca = b.Beca
	   AND b.Empresa = @Empresa
	   AND b.Ciclo = @CicloEscolar
	   AND b.FormaBeca = 'Facturable'
     GROUP BY a.Beca, a.Cliente, a.Moneda, a.TipoCambio, a.Unidad, a.Causa
        
    SELECT @Renglon = 2048.0, @RenglonID = 1 
       
    OPEN crBecaUnico
    FETCH NEXT FROM crBecaUnico INTO @BecaB, @Cliente, @Moneda, @TipoCambio, @Unidad, @CausaVenta
    WHILE (@@FETCH_STATUS = 0) AND (@Ok IS NULL)
    BEGIN
      SELECT @FacturarA = FacturarA,
		     @MovBeca = MovVenta,
			 @ConceptoBeca = ConceptoVenta
        FROM CEBecaCiclo
       WHERE Empresa = @Empresa
		AND Ciclo = @CicloEscolar
		AND Beca = @BecaB  
         
      IF @CalcularVenta <> 'Cuotas Kardex'
	  BEGIN
        INSERT Venta (
			    Empresa, Mov, FechaEmision, Concepto, Moneda, TipoCambio, Usuario, Condicion, Estatus, Directo, Prioridad,
				Cliente, Almacen, FechaRequerida, Sucursal, OrigenTipo, Origen, OrigenID, ContUso, Referencia,
				Causa, UEN, Observaciones)
        VALUES (@Empresa, @MovBeca, @FechaEmision, @ConceptoBeca, @Moneda, @TipoCambio, @Usuario, @CondicionPago, 'SINAFECTAR', 1, 'Normal',
				@Cliente, @Almacen, @FechaEmision, @Sucursal, 'CE', @Mov, @MovID, @CentroCosto, @AlumnoRefVTAS,
				@CausaVenta, @UEN, @AlumnoComVTAS)
      END ELSE BEGIN
        INSERT @CuotasKardex (
			    Empresa, Mov, FechaEmision, Concepto, Moneda, TipoCambio, Usuario, Condicion, Estatus, Directo, Prioridad,
				Cliente, Almacen, FechaRequerida, Vencimiento, Sucursal, OrigenTipo, Origen, OrigenID, ContUso, Referencia)
        VALUES (@Empresa, @MovBeca, @FechaEmision, @ConceptoBeca, @Moneda, @TipoCambio, @Usuario, @CondicionPago, 'SINAFECTAR', 1, 'Normal',
				@Cliente, @Almacen, @FechaEmision, @FechaEmision, @Sucursal, 'CE', @Mov, @MovID, @CentroCosto, @AlumnoRefVTAS)
      END

      IF @@ERROR <> 0 SELECT @Ok = 1
      SET @IDGenerarBeca = SCOPE_IDENTITY()     
      
      IF @Ok IS NULL
      BEGIN 

        DECLARE crBecaDetalle CURSOR FOR 
        SELECT a.Articulo, (a.Importe * -1), a.PagoNum, a.Pagos, a.ConceptoCuota
          FROM @TablaBeca a
		  JOIN CEBecaCiclo b ON a.Beca = b.Beca
		   AND b.Empresa = @Empresa
		   AND b.Ciclo=@CicloEscolar
		   AND b.FormaBeca = 'Facturable'
         WHERE a.Cliente = @Cliente
		   AND a.Beca = @BecaB
		   AND a.Moneda = @Moneda
		   AND a.TipoCambio = @TipoCambio
		   AND a.Unidad = @Unidad 
         GROUP BY a.Articulo, a.PagoNum, a.Pagos, a.ConceptoCuota ,a.Importe  
       
        SELECT @Renglon = 2048.0, @RenglonID = 1 
       
        OPEN crBecaDetalle
        FETCH NEXT FROM crBecaDetalle INTO @ArticuloB, @PrecioB, @PagoNumB, @PagosB, @ConceptoCuotaBD
        WHILE (@@FETCH_STATUS = 0) AND (@Ok IS NULL)
        BEGIN        
        
          SELECT @ArtTipo   = Tipo,
			     @Impuesto1 = Impuesto1,
			     @Impuesto2 = Impuesto2,
				 @Impuesto3 = Impuesto3
		    FROM Art
		   WHERE Articulo = @ArticuloB
          
		  EXEC spRenglonTipo @ArtTipo, NULL, @RenglonTipo OUTPUT  
          EXEC spZonaImp @ZonaImpuesto, @Impuesto1 OUTPUT
          EXEC spZonaImp @ZonaImpuesto, @Impuesto2 OUTPUT
          EXEC spZonaImp @ZonaImpuesto, @Impuesto3 OUTPUT
          EXEC spTipoImpuesto 'VTAS', @IDGenerarBeca, @MovBeca, @FechaEmision, @Empresa, 0, @FacturarA, NULL, @Articulo = @ArticuloB, @EnSilencio = 1, @Impuesto1 = @Impuesto1 OUTPUT, @Impuesto2 = @Impuesto2 OUTPUT, @Impuesto3 = @Impuesto3 OUTPUT
         
          SELECT @ZonaImpuesto = ZonaImpuesto FROM Cte WHERE Cliente = @Cliente  
             
          IF @CalcularVenta <> 'Cuotas Kardex' 
		  BEGIN
            INSERT VentaD (
				    ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, CantidadPendiente, Unidad, Almacen, Articulo, SubCuenta, Precio,
					PrecioMoneda, PrecioTipoCambio, FechaRequerida, Sucursal, CantidadInventario, Impuesto1, Impuesto2, Impuesto3, ContUso,DescripcionExtra)
            VALUES (@IDGenerarBeca, @Renglon, 0, @RenglonID, @RenglonTipo, 1, NULL, @Unidad, @Almacen, @ArticuloB, @SubCuenta, @PrecioB,
					@Moneda, @TipoCambio, @FechaEmision, @Sucursal, 1, @Impuesto1, @Impuesto2, @Impuesto3, @CentroCosto,convert (varchar(12),  @Fecha,103)+'114')
             
          END ELSE BEGIN
            INSERT @CuotasKardexD (
				    ID, Renglon, RenglonSub, RenglonID, RenglonTipo, Cantidad, CantidadPendiente, Unidad, Almacen, Articulo, SubCuenta, Precio,
				    FechaRequerida, Sucursal, CantidadInventario, Alumno, Impuesto1, Impuesto2, Impuesto3, ConceptoCuota, NumDocumento, ContUso)
            VALUES (@IDGenerarBeca, @Renglon, 0, @RenglonID, @RenglonTipo, 1, NULL, @Unidad, @Almacen, @ArticuloB, @SubCuenta, @PrecioB,
					@FechaEmision, @Sucursal, 1, @Alumno, @Impuesto1, @Impuesto2, @Impuesto3, @ConceptoCuotaBD, CONVERT(varchar,ISNULL(@PagoNumB,1))+'/'+CONVERT(varchar,ISNULL(@PagosB,1)), @CentroCosto)  
          END

          IF @@ERROR <> 0 SELECT @Ok = 1  
            
          SELECT @Renglon = @Renglon + 2048.0, @RenglonID = @RenglonID+1 
             
		  FETCH NEXT FROM crBecaDetalle INTO @ArticuloB, @PrecioB, @PagoNumB, @PagosB, @ConceptoCuotaBD
        END
        CLOSE crBecaDetalle
        DEALLOCATE crBecaDetalle    
                   
        IF @Ok IS NULL AND @IDGenerarBeca IS NOT NULL AND @CalcularVenta <> 'Cuotas Kardex'
        BEGIN              
		  IF @CalcularVenta = 'Movimiento'
			EXEC spAfectar 'VTAS', @IDGenerarBeca, 'AFECTAR', 'Todo', NULL, @Usuario, @EnSilencio = 1, @Conexion = 1, @Ok = @Ok OUTPUT, @OkRef = @OkRef OUTPUT
               
		  ELSE IF @CalcularVenta = 'Job'
		    EXEC spAfectar 'VTAS', @IDGenerarBeca, 'AFECTAR', 'Todo', NULL, @Usuario, @EnSilencio = 1, @Estacion = 2
             
          SELECT @MovIDGenerar = MovID FROM Venta WHERE ID = @IDGenerarBeca    
             
		  IF @Ok IS NULL
            EXEC spMovFlujo @Sucursal, 'AFECTAR', @Empresa, 'CE', @ID, @Mov, @MovID, 'VTAS', @IDGenerarBeca, @MovGenerar, @MovIDGenerar, @Ok OUTPUT
           
          IF @MovTipo IN ('CE.IE','CE.CEX')
            UPDATE CEInfoAlumno SET FechaUltimaCuotaIE = GETDATE() WHERE IDMov = @ID
        END
      END 
       
      FETCH NEXT FROM crBecaUnico INTO @BecaB, @Cliente, @Moneda, @TipoCambio, @Unidad, @CausaVenta
      END
    CLOSE crBecaUnico
    DEALLOCATE crBecaUnico
  END
END

  IF @CalcularVenta = 'Cuotas Kardex' AND EXISTS (SELECT * FROM @CuotasKardex)
  BEGIN
    SELECT a.Concepto ConceptoCuota,
		   a.Cliente Cliente,
		   CASE
			 WHEN EsBeca = 0
			 THEN (SELECT Descripcion1 FROM Art WHERE Articulo = b.Articulo)
			 ELSE b.Beca
		   END Descripcion,
		   b.Articulo Articulo,
		   b.Precio Precio,
		   b.Cantidad,
		   b.Impuesto1 Impuesto,
           a.Empresa,
		   a.Mov,
		   a.FechaEmision,
		   a.Concepto,
		   a.Moneda,
		   a.TipoCambio,
		   a.Usuario,
		   a.Condicion,
		   a.Estatus,
		   a.Directo,
		   a.Prioridad,
		   a.Almacen,
		   a.FechaRequerida,
		   a.Vencimiento,
		   a.Sucursal,
		   a.OrigenTipo,
		   a.Origen,
		   a.OrigenID,
		   a.DocFuente,
		   a.Referencia,
		   a.PorcRecargo,
		   a.ArtRecargo,
		   a.Frecuencia,
           b.Unidad,
		   b.SubCuenta,
		   b.CantidadInventario,
		   b.Alumno,
		   b.FechaVencimiento,
		   b.MovGenerar,
		   b.EsBeca,
		   b.Beca,
		   b.EstatusD,
		   b.Impuesto1,
		   b.Impuesto2,
		   b.Impuesto3,
		   b.NumDocumento,
		   b.DescripcionExtra,
		   b.ContUso,
		   b.RecSRec,
           '',
		    ((b.Precio * b.Cantidad) + (ISNULL(((b.Precio * b.Cantidad)*b.Impuesto1)/100,0))) Total
	  FROM @CuotasKardex a
	  JOIN @CuotasKardexD b ON a.ID = b.ID
     ORDER BY Cliente, Precio DESC
  END  
   
END

GO
